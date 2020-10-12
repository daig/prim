module Ref.Char8 where
import Ref.Byte

index# ∷ Ref → I {- ^ Offset in elments -} → Char8#
index# = indexCharOffAddr#

read# ∷ Ref → I → ST s Char8#
read# = readCharOffAddr#

write# ∷ Ref → I → Char8# → ST_ s
write# = writeCharOffAddr#
