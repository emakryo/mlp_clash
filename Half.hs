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
    | e == 0 = (-1)**s * 2**(1-fwidth-bias) * f
    | e == mb && f == 0 = (-1)**s * inf
    | e == mb = nan
    | otherwise = (-1)**s * 2**(e-fwidth-bias) * (f + 2**fwidth)
    where
    mb = maxBound :: Unsigned 5
    bias = fromIntegral $ shiftR mb 1
    fwidth = 10
    s = fromIntegral $ sign
    e = fromIntegral $ expo
    f = fromIntegral $ frac

-- NB, non-synthesizable
toHalf :: (Floating a, RealFrac a, RealFloat a) => a -> Half
toHalf x = (s, e, f)
    where
    mb = maxBound :: Unsigned 5
    bias = fromIntegral $ shiftR mb 1
    fwidth = 10
    s = if x >= 0 then 0 else 1 :: Bit
    e' = fromIntegral $ floor $ logBase 2 $ abs x
    e | x==0 || e' < (-bias) = 0
      | isInfinite x || isNaN x || e' > (fromIntegral $ mb-bias) = mb
      | otherwise = e'+bias
    f | e==maxBound || isInfinite x = 0
      | isNaN x = 1
      | otherwise = floor $ (abs x / (2**e')-1) * 2**fwidth

-- 3 clock latency
fromSigned :: Signal (Signed 16) -> Signal Half
fromSigned x = bundle (s3, e3, f3)
    where
    s1 = register 0 $ msb <$> x
    abs1 = register 0 $ (bitCoerce . abs) <$> x :: Signal (Unsigned 16)

    s2 = register 0 s1
    abs2 = register 0 abs1
    lz2 = register 0 $ (fromIntegral . countLeadingZeros) <$> abs1 :: Signal (Unsigned 5)

    s3 = register 0 $ s2
    e3 = register 0 $ mux (abs2 .==. 0) 0 (30-lz2)
    lz' = fromIntegral <$> lz2
    f' = shift <$> abs2 <*> (lz'-5)
    f3 = register 0 $ resize <$> f' :: Signal (Unsigned 10)

fromSignedTest xs = sampleN (L.length xs + 3) $ simulate fromSigned $ xs

concat# :: KnownNat n => KnownNat m => Unsigned n -> Unsigned m -> Unsigned (n+m)
concat# x y = unpack $ (pack x) ++# (pack y)

-- 3 clock latency
add# :: Signal Half -> Signal Half -> Signal Half
add# a b = f3 $ f2 $ f1 a b
    where
    f1 a b = register (unpack 0) $ add1 <$> a <*> b
    f2 x = register (unpack 0) $ add2 <$> x
    f3 x = register (unpack 0) $ add3 <$> x

type StAdd1 = (Bit, Bit, Unsigned 5, Unsigned 10, Unsigned 10, Unsigned 5)
add1 :: Half -> Half -> StAdd1
add1 a b = (op1, s1, ea1, fa1, fb1, de1)
    where
    (sa, ea, fa) = a
    (sb, eb, fb) = b
    op1 = xor sa sb
    cmp = (concat# ea fa) > (concat# eb fb)
    s1 | op1 == 0 = sa
       | cmp = sa
       | otherwise = sb
    (ea1, fa1, fb1, de1)
        | cmp = (ea, fa, fb, ea-eb)
        | otherwise = (eb, fb, fa, eb-ea)

type StAdd2 = (Bit, Unsigned 5, Unsigned 5, Unsigned 12)
add2 :: StAdd1 -> StAdd2
add2 (op1, s1, ea1, fa1, fb1, de1) = (s1, e2, lz2, f2)
    where
    extOne x = unpack $ (1 ::BitVector 2) ++# pack x
    fa' = extOne fa1
    fb' = shiftR (extOne fb1) (fromIntegral de1)
    f2 = if op1 == 0 then fa'+fb' else fa'-fb' :: Unsigned 12
    lz2 = fromIntegral $ countLeadingZeros f2
    e2 = if ea1 == 0 then 0 else ea1-lz2+1

add3 :: StAdd2 -> Half
add3 (s2, e2, lz2, f2) = (s2, e3, f3)
    where
    fp = shift f2 $ fromIntegral $ lz2-1
    f0 = shiftR f2 1
    f' = if lz2 == 0 then f0 else fp
    e3 = if f2 == 0 then 0 else e2
    f3 = unpack $ slice d9 d0 f'

-- 3 clock latency
mul# :: Signal Half -> Signal Half -> Signal Half
mul# a b = f3 $ f2 $ f1 a b
    where
    f1 a b = register (0, True, 0, 0) $ mul1 <$> a <*> b
    f2 x = register (0, True, False, False, 0, 0) $ mul2 <$> x
    f3 x = register (0, 0, 0) $ mul3 <$> x

type StMul1 = (Bit, Bool, Unsigned 6, Unsigned 11)
mul1 :: Half -> Half -> StMul1
mul1 a b = (s, ez, e, f)
    where
    extOne x = unpack $ (1 :: Bit) ++# (pack x)
    (sa, ea, fa) = a
    (sb, eb, fb) = b
    s = xor sa sb
    ez = (ea == 0) || (eb == 0)
    e = plus ea eb
    fa' = extOne fa :: Unsigned 11
    fb' = extOne fb :: Unsigned 11
    f' = times fa' fb' :: Unsigned 22
    f = unpack $ slice d21 d11 f'

type StMul2 = (Bit, Bool, Bool, Bool, Unsigned 5, Unsigned 10)
mul2 :: StMul1 -> StMul2
mul2 (s1, ez1, e1, f1) = (s1, ez1, c2, ov2, e2, f2)
    where
    e' = e1-15
    ov2 = msb e' == 1
    e2 = resize e'
    c2 = msb f1 == 1
    f' = if c2 then f1 else shift f1 1
    f2 = unpack $ slice d9 d0 f'

mul3 :: StMul2 -> Half
mul3 (s2, ez2, c2, ov2, e2, f2) = (s2, e3, f3)
    where
    e3 | ez2 = 0
       | ov2 = 31
       | c2 = e2+1
       | otherwise = e2
    f3 | ez2 || ov2 = 0
       | otherwise = f2

binOpTest binop (xs::[Double]) (ys::[Double]) = zs
    where
    binop' (x :: Signal (BitVector 32)) =
        let (a, b) = unbundle (split <$> x)
        in binop (unpack <$> a) (unpack <$> b)
    f = pack . toHalf
    zs = sampleN (L.length xs) $ simulate binop' $ L.zipWith (\x y -> f x ++# f y) xs ys

relu :: Signal Half -> Signal Half
relu x = register (0, 0, 0) $ bundle (0, e1, f1)
    where
    (s, e, f) = unbundle x
    e1 = mux (s .==. 0) e 0
    f1 = mux (s .==. 0) f 0

{-# ANN topEntity
   (defTop
    { t_name = "clash_top"
    , t_inputs = ["SW"]
    , t_outputs = ["LED"]
    }) #-}
topEntity :: Signal (Signed 10) -> Signal (BitVector 10)
topEntity x = fmap ((slice d15 d6) . pack) $ fromSigned $ fmap resize x
