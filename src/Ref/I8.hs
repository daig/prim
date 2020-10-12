module Ref.I8 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → I8#
index# = indexInt8OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s I8#
read# = readInt8OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → I8# → ST_ s
write# = writeInt8OffAddr#
