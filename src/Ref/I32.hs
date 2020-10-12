module Ref.I32 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → I32#
index# = indexInt32OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s I32#
read# = readInt32OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → I32# → ST_ s
write# = writeInt32OffAddr#
