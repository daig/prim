-- | Description : Packed 'Array.Byte' Arrays of 'I64'
module Array.I64 where
import Array.Byte


index# ∷ A → I {- ^ Offset in elments -} → I64
index# = indexInt64Array#

index## ∷ A → I {- ^ Offset in bytes -} → I64
index## = indexWord8ArrayAsInt64#

read#, readA# ∷ M s → I → ST# s I64
read# = readIntArray#
readA# = atomicReadIntArray#


write#,writeA# ∷ M s → I → I64 → ST_# s
write# = writeIntArray#
writeA# = atomicWriteIntArray#

cas# ∷ M s
    → I   -- ^ Source offset
    → I64 -- ^ Expected old value
    → I64 -- ^ New value
    → ST# s I64 -- ^ The actual old value
cas# = casIntArray#

fetchAdd ∷ M s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to add
         → ST# s I64 -- ^ The old value
fetchAdd = fetchAddIntArray#

fetchSub ∷ M s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to subtract
         → ST# s I64 -- ^ The old value
fetchSub = fetchSubIntArray#

fetchAnd ∷ M s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @and@
         → ST# s I64 -- ^ The old value
fetchAnd = fetchAndIntArray#

fetchNand ∷ M s -- ^ Source
          → I   -- ^ Source offset
          → I64 -- ^ Value to bitwise @nand@
          → ST# s I64 -- ^ The old value
fetchNand = fetchNandIntArray#
fetchOr  ∷ M s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @or@
         → ST# s I64 -- ^ The old value
fetchOr = fetchOrIntArray#

fetchXor ∷ M s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @xor@
         → ST# s I64 -- ^ The old value
fetchXor = fetchXorIntArray#
