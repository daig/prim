-- | Description : Packed 'Array.Byte' Arrays of 'F64'
module Array.F32 where
import Array.Byte

index# ∷ A → I → F32
index# = indexFloatArray#

read# ∷ M s → I → ST# s F32
read# = readFloatArray#

write# ∷ M s → I → F32 → ST_# s
write# = writeFloatArray#
