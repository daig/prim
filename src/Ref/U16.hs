module Ref.U16 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → U16#
index# = indexWord16OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s U16#
read# = readWord16OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → U16# → ST_ s
write# = writeWord16OffAddr#
