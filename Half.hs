module Half where

import CLaSH.Prelude
import GHC.List as L

-- Half = (sign, exponent, fraction)
type Half = (Bit, Unsigned 5, Unsigned 10)

nan = 0/0
inf = 1/0

-- NB. non-synthesizable
toFloating :: Floating f => Half -> f
toFloating (sign, expo, frac)
    | e == 0 = (-1)**b * 2**(-24) * f
    | b == 0 && e == 31 && f == 0 = inf
    | b == 1 && e == 31 && f == 0 = -inf
    | e == 31 = nan
    | otherwise = (-1)**b * 2**(e-25) * (f + 2**10)
    where
    b = fromIntegral $ sign
    e = fromIntegral $ expo
    f = fromIntegral $ frac

-- NB, non-synthesizable
toHalf :: (Floating a, RealFrac a, RealFloat a) => a -> Half
toHalf x = (s, e, f)
    where
    s = if x >= 0 then 0 else 1 :: Bit
    e' = (floor $ logBase 2 $ abs x) :: Integer
    e | x==0 = 0
      | isInfinite x || isNaN x = 31
      | otherwise = (fromInteger $ max (-15) $ min 16 e')+15
    f | e==31 || isInfinite x = 0
      | isNaN x = 1
      | otherwise = floor $ (abs x / (2**fromIntegral e')-1) * 2**10

-- 3 clock latency
fromSigned :: Signal (Signed 16) -> Signal Half
fromSigned x = bundle (s3, e3, f3)
    where
    s1 = register 0 $ msb <$> x
    abs1 = register 0 $ (bitCoerce . abs) <$> x :: Signal (Unsigned 16)

    s2 = register 0 s1
    abs2 = register 0 abs1
    lz2 = register 0 $ (fromIntegral . countLeadingZeros) <$> abs1 :: Signal (Unsigned 5)
    lz2' = fromIntegral <$> lz2
    e2 = mux (abs2 .==. 0) (signal 0) (30-lz2)
    f2 = shift <$> abs2 <*> (lz2'-5)

    s3 = register 0 $ s2
    e3 = register 0 $ e2
    f3 = register 0 $ resize <$> f2 :: Signal (Unsigned 10)

concat# :: KnownNat n => KnownNat m => Unsigned n -> Unsigned m -> Unsigned (n+m)
concat# x y = unpack $ (pack x) ++# (pack y)

-- 3 clock latency
add# :: Signal Half -> Signal Half -> Signal Half
add# a b = add3 $ add2 $ add1 a b

type StAdd1 = (Bit, Bit, Unsigned 5, Unsigned 10, Unsigned 10, Unsigned 5)
add1 :: Signal Half -> Signal Half -> Signal StAdd1
add1 a b = bundle (op1, s1, ea1, fa1, fb1, de1)
    where
    (sa, ea, fa) = unbundle a
    (sb, eb, fb) = unbundle b
    op = xor <$> sa <*> sb -- 0: add, 1: sub
    a' = concat# <$> ea <*> fa
    b' = concat# <$> eb <*> fb
    cmp = a' .>. b'
    op1 = register 0 op
    s1 = register 0 $ mux (op .==. 0) sa (mux cmp sa sb)
    ea1 = register 0 $ mux cmp ea eb
    fa1 = register 0 $ mux cmp fa fb
    fb1 = register 0 $ mux cmp fb fa
    de1 = register 0 $ mux cmp (ea-eb) (eb-ea)

type StAdd2 = (Bit, Unsigned 5, Unsigned 5, Unsigned 12)
add2 :: Signal StAdd1 -> Signal StAdd2
add2 x = bundle (s2, e2, lz2, f2)
    where
    (op1, s1, ea1, fa1, fb1, de1) = unbundle x
    one = 1 :: BitVector 2
    fa' = (\x -> unpack $ one ++# (pack x)) <$> fa1
    fb' = (\x y-> unpack $ shiftR (one ++# pack x) (fromIntegral y)) <$> fb1 <*> de1
    f' = mux (op1 .==. 0) (fa'+fb') (fa'-fb') :: Signal (Unsigned 12)
    lz = (fromIntegral . countLeadingZeros) <$> f'
    s2 = register 0 s1
    e2 = register 0 $ mux (ea1 .==. 0) 0 (ea1-lz+1)
    lz2 = register 0 lz
    f2 = register 0 f'

add3 :: Signal StAdd2 -> Signal Half
add3 x = bundle (s3, e3, f3)
    where
    (s2, e2, lz2, f2) = unbundle x
    f''p = (\x y -> shift x $ fromIntegral (y - 1)) <$> f2 <*> lz2
    f''0 = (\x -> shiftR x 1) <$> f2
    f'' = mux (lz2 .==. 0) f''0 f''p
    s3 = register 0 s2
    e3 = register 0 $ mux (f2 .==. 0) 0 e2
    f3 = register 0 $ (unpack . slice d9 d0) <$> f''

-- 3 clock latency
mul# :: Signal Half -> Signal Half -> Signal Half
mul# a b = mul3 $ mul2 $ mul1 a b

type StMul1 = (Bit, Bool, Unsigned 6, Unsigned 11)
mul1 :: Signal Half -> Signal Half -> Signal StMul1
mul1 a b = bundle (s1, ez1, e1, f1)
    where
    (sa, ea, fa) = unbundle a
    (sb, eb, fb) = unbundle b
    s1 = register 0 $ pack <$> (xor <$> sa <*> sb)
    ez1 = register True $ (ea .==. 0) .||. (eb .==. 0)
    e1 = register 0 $ plus <$> ea <*> eb
    extOne x = unpack $ (1 :: Bit) ++# (pack x)
    fa' = extOne <$> fa :: Signal (Unsigned 11)
    fb' = extOne <$> fb :: Signal (Unsigned 11)
    f' = times <$> fa' <*> fb' :: Signal (Unsigned 22)
    f1 = register 0 $ (unpack . slice d21 d11) <$> f'

type StMul2 = (Bit, Bool, Bool, Unsigned 5, Unsigned 10)
mul2 :: Signal StMul1 -> Signal StMul2
mul2 x = bundle (s2, ez2, c2, e2, f2)
    where
    (s1, ez1, e1, f1) = unbundle x
    s2 = register 0 s1
    ez2 = register True ez1
    e' = e1-15
    e2 = mux ((msb <$> e') .==. 0) (resize <$> e') 31
    c = (\x -> (msb x) == 1) <$> f1
    c2 = register True c
    f' = (\x -> shift x 1) <$> f1
    f'' = mux c f1 f'
    f2 = register 0 $ (unpack . slice d9 d0) <$> f''

mul3 :: Signal StMul2 -> Signal Half
mul3 x = bundle (s3, e3, f3)
    where
    (s2, ez2, c2, e2, f2) = unbundle x
    s3 = register 0 s2
    e' = mux c2 (e2+1) e2
    e'' = mux ez2 0 e'
    e3 = register 0 e''
    f3 = register 0 $ mux ez2 0 f2

test1 unop xs = sampleN (L.length xs) $ simulate unop $ toHalf <$> xs

test2 binop (xs::[Double]) (ys::[Double]) = zs
    where
    binop' (x :: Signal (BitVector 32)) =
        let (a, b) = unbundle (split <$> x)
        in binop (unpack <$> a) (unpack <$> b)
    f = pack . toHalf
    zs = sampleN (L.length xs) $ simulate binop' $ L.zipWith (\x y -> f x ++# f y) xs ys

{-# ANN topEntity
   (defTop
    { t_name = "clash_top"
    , t_inputs = ["SW"]
    , t_outputs = ["LED"]
    }) #-}
topEntity :: Signal (Signed 10) -> Signal (BitVector 10)
topEntity x = fmap ((slice d15 d6) . pack) $ fromSigned $ fmap resize x
