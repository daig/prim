module Ref.Char8 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → Char8#
index# = indexCharOffAddr#

read# ∷ Ref → I64 → ST s Char8#
read# = readCharOffAddr#

write# ∷ Ref → I64 → Char8# → ST_ s
write# = writeCharOffAddr#
