module Ref.Char where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → Char
index# = indexWideCharOffAddr#

read# ∷ Ref → I64 → ST s Char
read# = readWideCharOffAddr#

write# ∷ Ref → I64 → Char → ST_ s
write# = writeWideCharOffAddr#
