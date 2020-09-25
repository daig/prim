module Ref.F64 where
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → F64
index# = indexDoubleOffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s F64
read# = readDoubleOffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → F64 → ST_ s
write# = writeDoubleOffAddr#
