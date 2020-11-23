module P.P.Byte where
import P.Byte as Byte

index# ∷ P → I64 {- ^ Offset in elments -} → Byte.P
index# = coerce indexAddrOffAddr#

read# ∷ P → I64 {- ^ Offset in elements -} → ST# s Byte.P
read# = coerce readAddrOffAddr#

write# ∷ P → I64 {- ^ Offset in elements -} → Byte.P → ST_# s
write# = coerce writeAddrOffAddr#
