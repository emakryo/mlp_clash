module Half where

import CLaSH.Prelude
import GHC.List as L

type Half = (Bit, Unsigned 5, Unsigned 10)

sign :: Half -> Bit
sign (s, _, _) = s
expo :: Half -> Unsigned 5
expo (_, e, _) = e
frac :: Half -> Unsigned 10
frac (_, _, f) = f

-- NB. non-synthesizable
toFloating :: Floating f => Half -> f
toFloating x
    | e == 0 && f == 0 = 0
    | e == 0 = (-1)**b * 2**(-24) * f
    | otherwise = (-1)**b * 2**(e-25) * (f + 2**10)
    where
    b = fromIntegral $ sign x
    e = fromIntegral $ expo x
    f = fromIntegral $ frac x

-- NB, non-synthesizable
toHalf :: Floating a => RealFrac a => a -> Half
toHalf x = (s, e, f)
    where
    s = if x >= 0 then 0 else 1 :: Bit
    e' = (floor $ log (abs x) / log 2) :: Integer
    e  | x==0 = 0
       | otherwise = (fromInteger $ max (-15) $ min 16 e')+15
    f | e==31 = 0
      | otherwise = floor $ (abs x / (2**fromIntegral e')-1) * 2**10

-- 3 clock latency
fromSigned :: Signal (Signed 16) -> Signal Half
fromSigned x = bundle (s3, e3, f3)
    where
    s1 = register 0 $ fmap msb x
    abs1 = register 0 $ fmap (bitCoerce . abs) x :: Signal (Unsigned 16)

    s2 = register 0 s1
    abs2 = register 0 abs1
    lz2 = register 0 $ fmap (fromIntegral . countLeadingZeros) abs1 :: Signal (Unsigned 5)
    lz2' = fmap fromIntegral lz2
    e2 = mux (abs2 .==. 0) (signal 0) (30-lz2)
    f2 = liftA2 shift abs2 (lz2'-5)

    s3 = register 0 $ s2
    e3 = register 0 $ e2
    f3 = register 0 $ fmap resize f2 :: Signal (Unsigned 10)

concat# :: KnownNat n => KnownNat m => Unsigned n -> Unsigned m -> Unsigned (n+m)
concat# x y = unpack $ (pack x) ++# (pack y)

-- 3 clock latency
add# :: Signal Half -> Signal Half -> Signal Half
add# a b = add3 $ add2 $ add1 a b

type StAdd1 = (Bit, Bit, Unsigned 5, Unsigned 5, Unsigned 10, Unsigned 10, Unsigned 5)
add1 :: Signal Half -> Signal Half -> Signal StAdd1
add1 a b = bundle (op1, s1, ea1, eb1, fa1, fb1, de1)
    where
    (sa, ea, fa) = unbundle a
    (sb, eb, fb) = unbundle b
    op = liftA2 xor sa sb -- 0: add, 1: sub
    a' = liftA2 concat# ea fa
    b' = liftA2 concat# eb fb
    cmp = a' .>. b'
    op1 = register 0 op
    s1 = register 0 $ mux (op .==. 0) sa (mux cmp sa sb)
    ea1 = register 0 $ mux cmp ea eb
    fa1 = register 0 $ mux cmp fa fb
    eb1 = register 0 $ mux cmp eb ea
    fb1 = register 0 $ mux cmp fb fa
    de1 = register 0 $ mux cmp (ea-eb) (eb-ea)

type StAdd2 = (Bit, Unsigned 5, Unsigned 5, Unsigned 12)
add2 :: Signal StAdd1 -> Signal StAdd2
add2 x = bundle (s2, e2, lz2, f2)
    where
    (op1, s1, ea1, eb1, fa1, fb1, de1) = unbundle x
    fa' = fmap (\x -> unpack $ (1::BitVector 2) ++# (pack x)) fa1
    fb' = liftA2 (\x y-> unpack $ shiftR ((1::BitVector 2) ++# pack x) (fromIntegral y)) fb1 de1
    f' = mux (op1 .==. 0) (fa'+fb') (fa'-fb') :: Signal (Unsigned 12)
    lz = fmap (fromIntegral . countLeadingZeros) f' :: Signal (Unsigned 5)
    s2 = register 0 s1
    e2 = register 0 $ mux (ea1 .==. 0) 0 (ea1-lz+1)
    lz2 = register 0 lz
    f2 = register 0 f'

add3 :: Signal StAdd2 -> Signal Half
add3 x = bundle (s3, e3, f3)
    where
    (s2, e2, lz2, f2) = unbundle x
    f''p = liftA2 (\x y -> shift x $ fromIntegral (y - 1)) f2 lz2
    f''0 = fmap (\x -> shiftR x 1) f2
    f'' = mux (lz2 .==. 0) f''0 f''p
    s3 = register 0 s2
    e3 = register 0 $ mux (f2 .==. 0) 0 e2
    f3 = register 0 $ fmap (unpack . slice d9 d0) f'' :: Signal (Unsigned 10)

test1 unop xs = sampleN (L.length xs) $ simulate unop $ fmap toHalf xs

test2 binop (xs::[Double]) (ys::[Double]) = zs
    where
    binop' (x :: Signal (BitVector 32)) =
        let (a, b) = unbundle (fmap split x)
        in binop (fmap unpack a) (fmap unpack b)
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
