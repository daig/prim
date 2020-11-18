module Ref.Char8 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → Char8#
(!#) = indexCharOffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → Char8#
index# i r = r !# i

(!!#) ∷ Ref → I → ST# s Char8#
(!!#) = readCharOffAddr#
read# ∷ I → Ref → ST# s Char8#
read# i r = r !!# i

(¡#) ∷ Ref → I → Char8# → ST_# s
(¡#) = writeCharOffAddr#
write# ∷ I → Char8# → Ref → ST_# s
write# i x r = (r ¡# i) x
