module P.I8 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → I8#
(!#) = indexInt8OffAddr#
index# ∷ I {- ^ Offset in elments -} → P → I8#
index# i r = r !# i

(!!#) ∷ P → I {- ^ Offset in elements -} → ST# s I8#
(!!#) = readInt8OffAddr#
read# ∷ I {- ^ Offset in elments -} → P → ST# s I8#
read# i r = r !!# i

(¡#) ∷ P → I {- ^ Offset in elements -} → I8# → ST_# s
(¡#) = writeInt8OffAddr#
write# ∷ I {- ^ Offset in elements -} → I8# → P → ST_# s
write# i x r = (r ¡# i) x
