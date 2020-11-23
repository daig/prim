module P.I32 where
import P.Byte


(!#) ∷ P → I {- ^ Offset in elments -} → I32
(!#) = coerce indexInt32OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → I32
index# i r = r !# i


(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s I32
(!!#) = coerce readInt32OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s I32
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → I32 → ST_# s
(¡#) = coerce writeInt32OffAddr#
write# ∷ I {- ^ Offset in elements -} → I32 → P → ST_# s
write# i x r = (r ¡# i) x