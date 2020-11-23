module P.Char where
import P.Byte

index# ∷ P → I {- ^ Offset in elments -} → Char
index# = indexWideCharOffAddr#

read# ∷ P → I → ST# s Char
read# = readWideCharOffAddr#

write# ∷ P → I → Char → ST_# s
write# = writeWideCharOffAddr#
