module Ref.I64 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → I64
index# = indexInt64OffAddr#

read# ∷ Ref → I64 → ST s I64
read# = readInt64OffAddr#

write# ∷ Ref → I64 → I64 → ST_ s
write# = writeInt64OffAddr#
