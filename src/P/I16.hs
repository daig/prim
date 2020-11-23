module P.I16 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → I16
(!#) = coerce indexInt16OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → I16
index# i r = r !# i

(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s I16
(!!#) = coerce readInt16OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s I16
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → I16 → ST_# s
(¡#) = coerce writeInt16OffAddr#
write# ∷ I {- ^ Offset in elements -} → I16 → P → ST_# s
write# i x r = (r ¡# i) x
