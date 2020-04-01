module Array.I32 where
import Array.Byte

index# :: A
       -> I64 -- Offset in elments
       -> I32
index# = indexInt32Array#

index## :: A
        -> I64 -- Offset in bytes
        -> I32
index## = indexWord8ArrayAsInt32#

read# :: M s -> I64 -> ST s I32
read# = readInt32Array#

write# :: M s -> I64 -> I32 -> ST_ s
write# = writeInt32Array#
