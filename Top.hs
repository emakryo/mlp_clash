module Top where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit
import Half

type ClkMem = Clk "mem" 1000
type ClkAdc = Clk "adc" 1000

type SigMem a = Signal' ClkMem a
type SigAdc a = Signal' ClkAdc a

topEntity ::
    SigAdc Bit -- valid
    -> SigAdc Bit -- on
    -> SigAdc Bit -- first
    -> SigAdc Bit -- last
    -> SigAdc (Signed 16) -- data input
    -> SigMem Bit -- wren
    -> SigMem (Unsigned 4) -- addr
    -> SigMem (Unsigned 16) -- addr_j
    -> SigMem (Unsigned 16) -- addr_k
    -> SigMem (BitVector 16) -- write
    -> (SigAdc (BitVector 16), SigAdc Bit, SigMem (BitVector 16)) 

topEntity valid on first last x wren addr addrj addrk wx
    = (0, 0, 0)

--topEntity :: SigMem Half -> (SigMem Half, SigAdc (Unsigned 8))
--topEntity x = (register' sclock (toHalf 0) x, s)
--    where
--    s = register' sclock 0 (s+1)
