module Array.F64 where
import Array.Byte

index# :: Array -> I64 -> F64
index# = indexDoubleArray#

read# :: Mutable s -> I64 -> ST s F64
read# = readDoubleArray#

write# :: Mutable s -> I64 -> F64 -> ST_ s
write# = writeDoubleArray#
