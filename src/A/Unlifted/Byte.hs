module A.Unlifted.Byte where
import qualified Bytes
import Refs

index# ∷ A → I → Bytes.A
index# = indexByteArrayArray#

read# ∷ MA s → I → ST# s Bytes.A
read# = readByteArrayArray#

write# ∷ MA s → I → Bytes.A → ST_# s
write# = writeByteArrayArray#
