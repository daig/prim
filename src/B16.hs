module B16 where

fromU64 :: U64 -> B16
fromU64 = narrow16Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B16 -> B16
popCnt = popCnt16#; clz = clz16#; ctz = ctz16#

byteSwap :: B16 -> B16
byteSwap = byteSwap16#
pext :: B16 -> U64 -> B16
pext y x = pext16# x y
pdep :: B16 -> U64 -> U64
pdep y x = pdep16# x y

-- | Reverse the order of the bits.
reverse :: B16 -> B16
reverse = bitReverse16#
