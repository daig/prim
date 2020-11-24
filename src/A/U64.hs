module A.U64 where
import A

index# ∷ A → I {- ^ Offset in elments -} → U64
index# = coerce indexWord64Array#

index## ∷ A → I {- ^ Offset in bytes -} → U64
index## = coerce indexWord8ArrayAsWord64#

read# ∷ MA s → I → ST# s U64
read# = coerce readWord64Array#

write# ∷ MA s → I → U64 → ST_# s
write# = coerce writeWord64Array#
