module Ref.Ref.Byte where
import Ref.Byte as Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → Byte.Ref
index# = indexAddrOffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST# s Byte.Ref
read# = readAddrOffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → Byte.Ref → ST_# s
write# = writeAddrOffAddr#
