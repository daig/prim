module Array.F32 where
import Array.Byte

index# :: Array -> I64 -> F32
index# = indexFloatArray#

read# :: Mutable s -> I64 -> ST s F32
read# = readFloatArray#

write# :: Mutable s -> I64 -> F32 -> ST_ s
write# = writeFloatArray#
