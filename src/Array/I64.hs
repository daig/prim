module Array.I64 where
import Array.Byte

index# :: Array
       -> I64 -- Offset in elments
       -> I64
index# = indexInt64Array#

index## :: Array
        -> I64 -- Offset in bytes
        -> I64
index## = indexWord8ArrayAsInt64#

read#, readA# :: Mutable s -> I64 -> ST s I64
read# = readIntArray#
readA# = atomicReadIntArray#


write#,writeA# :: Mutable s -> I64 -> I64 -> ST_ s
write# = writeIntArray#
writeA# = atomicWriteIntArray#

cas# :: Mutable s
    -> I64 -- ^ Source offset
    -> I64 -- ^ Expected old value
    -> I64 -- ^ New value
    -> ST s I64 -- ^ The actual old value
cas# = casIntArray#

fetchAdd :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to add
         -> ST s I64 -- ^ The old value
fetchAdd = fetchAddIntArray#

fetchSub :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to subtract
         -> ST s I64 -- ^ The old value
fetchSub = fetchSubIntArray#

fetchAnd :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to bitwise @and@
         -> ST s I64 -- ^ The old value
fetchAnd = fetchAndIntArray#

fetchNand :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to bitwise @nand@
         -> ST s I64 -- ^ The old value
fetchNand = fetchNandIntArray#
fetchOr :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to bitwise @or@
         -> ST s I64 -- ^ The old value
fetchOr = fetchOrIntArray#

fetchXor :: Mutable s -- ^ Source
         -> I64 -- ^ Source offset
         -> I64 -- ^ Value to bitwise @xor@
         -> ST s I64 -- ^ The old value
fetchXor = fetchXorIntArray#
