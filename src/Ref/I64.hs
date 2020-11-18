module Ref.I64 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → I64
(!#) = indexInt64OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → I64
index# i r = r !# i

(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s I64
(!!#) = readInt64OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s I64
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → I64 → ST_# s
(¡#) = writeInt64OffAddr#
write# ∷ I {- ^ Offset in elements -} → I64 → Ref → ST_# s
write# i x r = (r ¡# i) x
