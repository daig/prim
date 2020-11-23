module P.F64 where
import P.Byte

index# ∷ P → I {- ^ Offset in elments -} → F64
index# = indexDoubleOffAddr#

read# ∷ P → I {- ^ Offset in elements -} → ST# s F64
read# = readDoubleOffAddr#

write# ∷ P → I {- ^ Offset in elements -} → F64 → ST_# s
write# = writeDoubleOffAddr#
