module B16 where

fromWord :: Word -> B16
fromWord = narrow16Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B16 -> B16
popCnt = popCnt16#; clz = clz16#; ctz = ctz16#

byteSwap :: B16 -> B16
byteSwap = byteSwap16#
pext :: B16 -> Word -> B16
pext y x = pext16# x y
pdep :: B16 -> Word -> Word
pdep y x = pdep16# x y
