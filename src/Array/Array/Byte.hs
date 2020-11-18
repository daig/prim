module Array.Array.Byte where
import qualified Array
import Array.Array

index# ∷ A → I → Array.Byte
index# = indexByteArrayArray#

read# ∷ M s → I → ST# s Array.Byte
read# = readByteArrayArray#

write# ∷ M s → I → Array.Byte → ST_# s
write# = writeByteArrayArray#
