module B32 where

fromWord :: Word -> B32
fromWord = narrow32Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B32 -> B32
popCnt = popCnt32#; clz = clz32#; ctz = ctz32#

byteSwap :: B32 -> B32
byteSwap = byteSwap32#

pext :: B32 -> Word -> B32
pext y x = pext32# x y
pdep :: B32 -> Word -> Word
pdep y x = pdep32# x y
