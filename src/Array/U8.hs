module Array.U8 where
import Array.Byte

index# :: Array -> I64 -> U8
index# = indexWord8Array#

read# :: Mutable s -> I64 -> ST s U8
read# = readWord8Array#

write# :: Mutable s -> I64 -> U8 -> ST_ s
write# = writeWord8Array#
