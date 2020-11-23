module P.U8 where
import P.Byte
(!#) ∷ P → I {- ^ Offset in elments -} → U8
(!#) = coerce indexWord8OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → U8
index# i r = r !# i


(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s U8
(!!#) = coerce readWord8OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s U8
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → U8 → ST_# s
(¡#) = coerce writeWord8OffAddr#
write# ∷ I {- ^ Offset in elements -} → U8 → P → ST_# s
write# i x r = (r ¡# i) x
