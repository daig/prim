module Ref.U8 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → U8#
index# = indexWord8OffAddr#

read# ∷ Ref → I {- ^ Offset in elements -} → ST s U8#
read# = readWord8OffAddr#

write# ∷ Ref → I {- ^ Offset in elements -} → U8# → ST_ s
write# = writeWord8OffAddr#
