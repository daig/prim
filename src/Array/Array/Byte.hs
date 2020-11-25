module Array.Array.Byte where
import qualified A.Raw as Byte
import Array.Array

index# ∷ A → I → Byte.A
index# = indexByteArrayArray#

read# ∷ MA s → I → ST# s Byte.A
read# = readByteArrayArray#

write# ∷ MA s → I → Byte.A → ST_# s
write# = writeByteArrayArray#
