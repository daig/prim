module Ref.Char where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → Char
index# = indexWideCharOffAddr#

read# ∷ Ref → I → ST s Char
read# = readWideCharOffAddr#

write# ∷ Ref → I → Char → ST_ s
write# = writeWideCharOffAddr#
