module Ref.I64 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → I64
index# = indexInt64OffAddr#

read# ∷ Ref → I → ST s I64
read# = readInt64OffAddr#

write# ∷ Ref → I → I64 → ST_ s
write# = writeInt64OffAddr#
