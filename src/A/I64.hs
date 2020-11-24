-- | Description : Packed 'Array.Byte' Arrays of 'I64'
module A.I64 where
import A


index# ∷ A → I {- ^ Offset in elments -} → I64
index# = coerce indexInt64Array#

index## ∷ A → I {- ^ Offset in bytes -} → I64
index## = coerce indexWord8ArrayAsInt64#

read#, readA# ∷ MA s → I → ST# s I64
read# = coerce readIntArray#
readA# = coerce atomicReadIntArray#


write#,writeA# ∷ MA s → I → I64 → ST_# s
write# = coerce writeIntArray#
writeA# = coerce atomicWriteIntArray#

cas# ∷ MA s
    → I   -- ^ Source offset
    → I64 -- ^ Expected old value
    → I64 -- ^ New value
    → ST# s I64 -- ^ The actual old value
cas# = coerce casIntArray#

fetchAdd ∷ MA s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to add
         → ST# s I64 -- ^ The old value
fetchAdd = coerce fetchAddIntArray#

fetchSub ∷ MA s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to subtract
         → ST# s I64 -- ^ The old value
fetchSub = coerce fetchSubIntArray#

fetchAnd ∷ MA s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @and@
         → ST# s I64 -- ^ The old value
fetchAnd = coerce fetchAndIntArray#

fetchNand ∷ MA s -- ^ Source
          → I   -- ^ Source offset
          → I64 -- ^ Value to bitwise @nand@
          → ST# s I64 -- ^ The old value
fetchNand = coerce fetchNandIntArray#
fetchOr  ∷ MA s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @or@
         → ST# s I64 -- ^ The old value
fetchOr = coerce fetchOrIntArray#

fetchXor ∷ MA s -- ^ Source
         → I   -- ^ Source offset
         → I64 -- ^ Value to bitwise @xor@
         → ST# s I64 -- ^ The old value
fetchXor = coerce fetchXorIntArray#
