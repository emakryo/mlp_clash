module AFloat where

import CLaSH.Prelude
import Data.Number.Transfinite

-- Arbitrary length floating point number
data AFloat n m = AFloat { sign :: Bit
                         , expo :: Unsigned n
                         , frac :: Unsigned m} deriving (Eq)


toFloat :: KnownNat n => KnownNat m => Floating a => Transfinite a => AFloat n m -> a
toFloat AFloat {sign=s, expo=e, frac=f}
    | (e == 0 && f == 0) = 0
    | (e == 0) = s' * f' / d
    | (e == maxBound && f == 0) = s' * infinity
    | (e == maxBound) = notANumber
    | otherwise = s' * e' * (1 + f' / d)
    where
    s' = (-1) ** (fromIntegral s)
    f' = fromIntegral f
    b = 2**(fromIntegral (finiteBitSize e)-1)-1
    e' = 2**(fromIntegral e - b)
    d = 2 ** fromIntegral (finiteBitSize f)

fromBits :: KnownNat n => KnownNat m => BitVector (n+m+3) -> SNat (n+1) -> SNat (m+1) -> AFloat (n+1) (m+1)
fromBits x n m = AFloat { sign=x!snatToInteger (addSNat n m)
                        , expo=unpack $ slice (subSNat (addSNat n m) d1) m x
                        , frac=unpack $ slice (subSNat m d1) d0 x}

instance (KnownNat n, KnownNat m) => Show (AFloat n m) where
    show x = show $ (toFloat x :: Double)
