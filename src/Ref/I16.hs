module Ref.I16 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → I16#
(!#) = indexInt16OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → I16#
index# i r = r !# i

(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s I16#
(!!#) = readInt16OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s I16#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → I16# → ST_# s
(¡#) = writeInt16OffAddr#
write# ∷ I {- ^ Offset in elements -} → I16# → Ref → ST_# s
write# i x r = (r ¡# i) x
