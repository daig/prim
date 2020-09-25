module Ref.I16 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → I16
index# = indexInt16OffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s I16
read# = readInt16OffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → I16 → ST_ s
write# = writeInt16OffAddr#
