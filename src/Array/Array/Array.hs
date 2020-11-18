module Array.Array.Array where
import qualified Array
import Array.Array

index# ∷ A → I → Array.Unlifted
index# = indexArrayArrayArray#

read# ∷ M s → I → ST# s Array.Unlifted
read# = readArrayArrayArray#

write# ∷ M s → I → Array.Unlifted → ST_# s
write# = writeArrayArrayArray#
