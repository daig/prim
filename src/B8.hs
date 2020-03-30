module B8 where

fromWord :: Word -> B8
fromWord = narrow8Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B8 -> B8
popCnt = popCnt8#; clz = clz8#; ctz = ctz8#

pext :: B8 -> Word -> B8
pext y x = pext8# x y
pdep :: B8 -> Word -> Word
pdep y x = pdep8# x y
