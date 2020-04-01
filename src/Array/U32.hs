module Array.U32 where
import Array.Byte

index# :: Array
       -> I64 -- Offset in elments
       -> U32
index# = indexWord32Array#

index## :: Array
        -> I64 -- Offset in bytes
        -> U32
index## = indexWord8ArrayAsWord32#

read# :: Mutable s -> I64 -> ST s U32
read# = readWord32Array#

write# :: Mutable s -> I64 -> U32 -> ST_ s
write# = writeWord32Array#
