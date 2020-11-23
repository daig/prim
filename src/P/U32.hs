module P.U32 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → U32
(!#) = coerce indexWord32OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → U32
index# i r = r !# i

(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s U32
(!!#) = coerce readWord32OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s U32
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → U32 → ST_# s
(¡#) = coerce writeWord32OffAddr#
write# ∷ I {- ^ Offset in elements -} → U32 → P → ST_# s
write# i x r = (r ¡# i) x
