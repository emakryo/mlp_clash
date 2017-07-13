module AFloat where

import CLaSH.Prelude
import CLaSH.Signal.Internal (Signal')
import Data.Number.Transfinite
import Data.Maybe (fromMaybe)

-- Arbitrary length floating point number
type AFloat n m = (Bit, Unsigned n, Unsigned m) -- (sign, expo, frac)
--data AFloat n m = AFloat { sign :: Bit
--                         , expo :: Unsigned n
--                         , frac :: Unsigned m} deriving (Eq)
sign (s,_,_) = s
expo (_,e,_) = e
frac (_,_,f) = f

type Half = AFloat 5 10

toFloat :: KnownNat n => KnownNat m => Floating a => Transfinite a => AFloat n m -> a
toFloat (s, e, f)
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

fromBits :: KnownNat n => KnownNat m => BitVector (1+(n+m)) -> AFloat n m
fromBits x = (unpack s, unpack e, unpack f)
    where
    (s, ef) = split x
    (e, f) = split ef

-- convert signed integer to AFloat after 3 clock
fromSigned :: forall n m l. KnownNat n => KnownNat m => KnownNat l => SNat (n+1) -> SNat (m+1) -> Signal (Signed (l+1)) -> Signal (AFloat (n+1) (m+1))
fromSigned dn dm x = af3
   where
   n' = snatToNum dn
   m' = snatToNum dm
   l' = fmap finiteBitSize x
   sign1 = register 0 $ fmap msb x
   abs1 = register 0 $ abs x
   sign2 = register 0 sign1
   abs2 = register 0 abs2
   clz = (max fromIntegral . countLeadingZeros :: Signed (l+1) -> Signed (n+1)
   dx2 = register 0 $ fmap clz abs1 :: Signal (Signed (n+1))
   ebias = fmap fromIntegral $ l'+(signal $ (shift 1 (n'-1))-2)
   e3 = fmap bitCoerce $ ebias-dx2 :: Signal (Unsigned (n+1))
   f3 = liftA2 shift abs2 (1+m'-l'+fmap fromIntegral dx2) :: Signal (Signed (l+1))
   f3' = fmap (resize . bitCoerce) f3
   af3 = register (fromBits 0) $ bundle (sign2, e3, f3')

halfFromSigned = fromSigned d5 d10

-- topEntity :: Signal (BitVector 16) -> Signal (BitVector 16)
-- topEntity x = register 0 $ fmap (resize . fromIntegral . countLeadingZeros) x
