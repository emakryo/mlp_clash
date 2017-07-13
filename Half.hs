module Half where

import CLaSH.Prelude

type Half = (Bit, Unsigned 5, Unsigned 10)

sign :: Half -> Bit
sign (s, _, _) = s
expo :: Half -> Unsigned 5
expo (_, e, _) = e
frac :: Half -> Unsigned 10
frac (_, _, f) = f

toFloating :: Floating f => Half -> f
toFloating x
    | e == 0 && f == 0 = 0
    | e == 0 = (-1)**b * 2**(-24) * f
    | otherwise = (-1)**b * 2**(e-25) * (f + 2**10)
    where
    b = fromIntegral $ sign x
    e = fromIntegral $ expo x
    f = fromIntegral $ frac x

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
    e2 = mux (fmap (==0) abs2) (signal 0) (30-lz2)
    f2 = liftA2 shift abs2 (lz2'-5)

    s3 = register 0 $ s2
    e3 = register 0 $ e2
    f3 = register 0 $ fmap resize f2 :: Signal (Unsigned 10)

--hadd :: Signal Half -> Signal Half -> Signal Half
--hadd x y = bundle (s3, e3, f3)
--    where
--    op1 = register 0 $ 
--
topEntity :: Signal (Signed 16) -> Signal (BitVector 16)
topEntity x = fmap pack $ fromSigned x
