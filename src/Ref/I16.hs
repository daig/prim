module Ref.I16 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → I16#
index# = indexInt16OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s I16#
read# = readInt16OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → I16# → ST_ s
write# = writeInt16OffAddr#
