module B8 (B8, module B8) where

fromU64 :: U64 -> B8
fromU64 = narrow8Word#

-- | Count the number of set bits
popCnt,clz,ctz :: B8 -> B8
popCnt = popCnt8#; clz = clz8#; ctz = ctz8#

pext :: B8 -> U64 -> B8
pext y x = pext8# x y
pdep :: B8 -> U64 -> U64
pdep y x = pdep8# x y

-- | Reverse the order of the bits.
reverse :: B8 -> B8
reverse = bitReverse8#
