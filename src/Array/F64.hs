-- | Description : Packed 'Array.Byte' Arrays of 'F64'
module Array.F64 where
import Array.Byte

index# ∷ A → I → F64
index# = indexDoubleArray#

read# ∷ M s → I → ST s F64
read# = readDoubleArray#

write# ∷ M s → I → F64 → ST_ s
write# = writeDoubleArray#
