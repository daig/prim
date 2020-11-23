module P.U16 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → U16
(!#) = coerce indexWord16OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → U16
index# i r = r !# i


(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s U16
(!!#) = coerce readWord16OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s U16
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → U16 → ST_# s
(¡#) = coerce writeWord16OffAddr#
write# ∷ I {- ^ Offset in elements -} → U16 → P → ST_# s
write# i x r = (r ¡# i) x
