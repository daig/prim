module Ref.I8 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → I8
index# = indexInt8OffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s I8
read# = readInt8OffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → I8 → ST_ s
write# = writeInt8OffAddr#
