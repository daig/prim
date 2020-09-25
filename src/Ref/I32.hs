module Ref.I32 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → I32
index# = indexInt32OffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s I32
read# = readInt32OffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → I32 → ST_ s
write# = writeInt32OffAddr#
