module Ref.I32 where
import Ref.Byte


(!#) ∷ Ref → I {- ^ Offset in elments -} → I32#
(!#) = indexInt32OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → I32#
index# i r = r !# i


(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s I32#
(!!#) = readInt32OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s I32#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → I32# → ST_# s
(¡#) = writeInt32OffAddr#
write# ∷ I {- ^ Offset in elements -} → I32# → Ref → ST_# s
write# i x r = (r ¡# i) x
