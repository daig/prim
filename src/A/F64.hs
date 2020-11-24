-- | Description : Packed 'Array.Byte' Arrays of 'F64'
module A.F64 where
import A

index# ∷ A → I → F64
index# = indexDoubleArray#

read# ∷ MA s → I → ST# s F64
read# = readDoubleArray#

write# ∷ MA s → I → F64 → ST_# s
write# = writeDoubleArray#
