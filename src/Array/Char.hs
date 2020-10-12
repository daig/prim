module Array.Char where
import Array.Byte

index# ∷ A → I {- ^ Offset in 4-byte words -} → Char
index# = indexWideCharArray#

index## ∷ A → I {- ^ Offset in bytes -} → Char
index## = indexWord8ArrayAsWideChar#


read# ∷ M s → I → ST s Char
read# = readWideCharArray#

write# ∷ M s → I → Char → ST_ s
write# = writeWideCharArray#
