module P.U64 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → U64
(!#) = coerce indexWord64OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → U64
index# i r = r !# i

(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s U64
(!!#) = coerce readWord64OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s U64
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → U64 → ST_# s
(¡#) = coerce writeWord64OffAddr#
write# ∷ I {- ^ Offset in elements -} → U64 → P → ST_# s
write# i x r = (r ¡# i) x
