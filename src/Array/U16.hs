module Array.U16 where
import Array.Byte

index# :: A
       -> I64 -- Offset in elments
       -> U16
index# = indexWord16Array#

index## :: A
        -> I64 -- Offset in bytes
        -> U16
index## = indexWord8ArrayAsWord16#


read# :: M s -> I64 -> ST s U16
read# = readWord8Array#

write# :: M s -> I64 -> U16 -> ST_ s
write# = writeWord8Array#
