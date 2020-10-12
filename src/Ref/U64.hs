module Ref.U64 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → U64
index# = indexWord64OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s U64
read# = readWord64OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → U64 → ST_ s
write# = writeWord64OffAddr#
