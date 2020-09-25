module Array.F32 where
import Array.Byte

index# ∷ A → I64 → F32
index# = indexFloatArray#

read# ∷ M s → I64 → ST s F32
read# = readFloatArray#

write# ∷ M s → I64 → F32 → ST_ s
write# = writeFloatArray#
