module P.Char8 where
import P.Byte

(!#) ∷ P → I {- ^ Offset in elments -} → Char8#
(!#) = indexCharOffAddr#
index# ∷ I {- ^ Offset in elments -} → P → Char8#
index# i r = r !# i

(!!#) ∷ P → I → ST# s Char8#
(!!#) = readCharOffAddr#
read# ∷ I → P → ST# s Char8#
read# i r = r !!# i

(¡#) ∷ P → I → Char8# → ST_# s
(¡#) = writeCharOffAddr#
write# ∷ I → Char8# → P → ST_# s
write# i x r = (r ¡# i) x
