module Array.Char where
import Array.Byte
import Char

index# ∷ A → I {- ^ Offset in 4-byte words -} → Char
index# = indexWideCharArray#

index## ∷ A → I {- ^ Offset in bytes -} → Char
index## = indexWord8ArrayAsWideChar#


read# ∷ MA s → I → ST# s Char
read# = readWideCharArray#

write# ∷ MA s → I → Char → ST_# s
write# = writeWideCharArray#
