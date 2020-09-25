module Array.F64 where
import Array.Byte

index# ∷ A → I64 → F64
index# = indexDoubleArray#

read# ∷ M s → I64 → ST s F64
read# = readDoubleArray#

write# ∷ M s → I64 → F64 → ST_ s
write# = writeDoubleArray#
