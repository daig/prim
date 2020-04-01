module Array.Ptr.Stable where
import Array.Byte
import Stable (Ptr)

index# :: A
       -> I64 -- Offset in elments
       -> Ptr a
index# = indexStablePtrArray#

index## :: A
        -> I64 -- Offset in bytes
        -> Ptr a
index## = indexWord8ArrayAsStablePtr#

read# :: M s -> I64 -> ST s (Ptr a)
read# = readStablePtrArray#

write# :: M s -> I64 -> Ptr a -> ST_ s
write# = writeStablePtrArray#
