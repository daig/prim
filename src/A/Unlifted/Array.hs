module A.Unlifted.Array where
import Refs

index# ∷ A → I → A
index# = indexArrayArrayArray#

read# ∷ MA s → I → ST# s A
read# = readArrayArrayArray#

write# ∷ MA s → I → A → ST_# s
write# = writeArrayArrayArray#
