module P.F32 where
import P.Byte

index# ∷ P → I {- ^ Offset in elments -} → F32
index# = indexFloatOffAddr#

read# ∷ P → I {- ^ Offset in elements -} → ST# s F32
read# = readFloatOffAddr#

write# ∷ P → I {- ^ Offset in elements -} → F32 → ST_# s
write# = writeFloatOffAddr#
