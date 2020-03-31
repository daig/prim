module B32 where

fromU64 :: U64 -> B32
fromU64 = narrow32Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B32 -> B32
popCnt = popCnt32#; clz = clz32#; ctz = ctz32#

byteSwap :: B32 -> B32
byteSwap = byteSwap32#

pext :: B32 -> U64 -> B32
pext y x = pext32# x y
pdep :: B32 -> U64 -> U64
pdep y x = pdep32# x y

-- | Reverse the order of the bits.
reverse :: B32 -> B32
reverse = bitReverse32#
