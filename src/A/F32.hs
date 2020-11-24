-- | Description : Packed 'Array.Byte' Arrays of 'F64'
module A.F32 where
import A

index# ∷ A → I → F32
index# = indexFloatArray#

read# ∷ MA s → I → ST# s F32
read# = readFloatArray#

write# ∷ MA s → I → F32 → ST_# s
write# = writeFloatArray#
