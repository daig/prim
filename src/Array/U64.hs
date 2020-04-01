module Array.U64 where
import Array.Byte

index# :: Array
       -> I64 -- Offset in elments
       -> U64
index# = indexWord64Array#

index## :: Array
        -> I64 -- Offset in bytes
        -> U64
index## = indexWord8ArrayAsWord64#

read# :: Mutable s -> I64 -> ST s U64
read# = readWord8Array#

write# :: Mutable s -> I64 -> U64 -> ST_ s
write# = writeWord8Array#
