module Array.U16 where
import Array.Byte

index# :: Array
       -> I64 -- Offset in elments
       -> U16
index# = indexWord16Array#

index## :: Array
        -> I64 -- Offset in bytes
        -> U16
index## = indexWord8ArrayAsWord16#


read# :: Mutable s -> I64 -> ST s U16
read# = readWord8Array#

write# :: Mutable s -> I64 -> U16 -> ST_ s
write# = writeWord8Array#
