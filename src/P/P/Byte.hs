module P.P.Byte where
import P.Byte as Byte

index# ∷ P → I64 {- ^ Offset in elments -} → Byte.P
index# = indexAddrOffAddr#

read# ∷ P → I64 {- ^ Offset in elements -} → ST# s Byte.P
read# = readAddrOffAddr#

write# ∷ P → I64 {- ^ Offset in elements -} → Byte.P → ST_# s
write# = writeAddrOffAddr#
