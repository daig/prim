module P.I64 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → I64
(!#) = coerce indexInt64OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → I64
index# i r = r !# i

(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s I64
(!!#) = coerce readInt64OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s I64
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → I64 → ST_# s
(¡#) = coerce writeInt64OffAddr#
write# ∷ I {- ^ Offset in elements -} → I64 → P → ST_# s
write# i x r = (r ¡# i) x
