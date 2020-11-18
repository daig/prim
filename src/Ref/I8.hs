module Ref.I8 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → I8#
(!#) = indexInt8OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → I8#
index# i r = r !# i

(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s I8#
(!!#) = readInt8OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s I8#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → I8# → ST_# s
(¡#) = writeInt8OffAddr#
write# ∷ I {- ^ Offset in elements -} → I8# → Ref → ST_# s
write# i x r = (r ¡# i) x
