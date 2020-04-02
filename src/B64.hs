module B64 where
import qualified GHC.Types as GHC

eq, ne :: B64 -> B64 -> B
eq x y = eqWord# x y
ne x y = neWord# x y

fromInt :: Int -> B64
fromInt = int2Word#
toInt :: B64 -> Int
toInt = word2Int#

toF32 :: B64 -> F32
toF32 = word2Float#
toF64 :: B64 -> F64
toF64 = word2Double#

toU8 :: B64 -> U8
toU8 = narrow8Word#
toU16 :: B64 -> U16
toU16 = narrow16Word#
toU32 :: B64 -> U32
toU32 = narrow32Word#

toB8 :: B64 -> B8
toB8 = narrow8Word#
toB16 :: B64 -> B16
toB16 = narrow16Word#
toB32 :: B64 -> B32
toB32 = narrow32Word#

and,or,xor :: B64 -> B64 -> B64
and = and#
or = or#
xor = xor#
not :: B64 -> B64
not = not#

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL#, shiftRL# :: Int -> B64 -> B64
shiftL# i w = uncheckedShiftL# w i

-- |Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftRL# i w = uncheckedShiftRL# w i

-- | Count the number of set bits
popCnt,clz,ctz :: B64 -> U8
popCnt = popCnt#; clz = clz#; ctz = ctz#

byteSwap :: B64 -> B64
byteSwap = byteSwap#
pdep, pext :: B64 -> B64 -> B64
pdep y x = pdep# x y; pext y x = pext# x y

-- | Reverse the order of the bits.
reverse :: B64 -> B64
reverse = bitReverse#
