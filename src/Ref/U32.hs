module Ref.U32 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → U32#
(!#) = indexWord32OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → U32#
index# i r = r !# i

(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s U32#
(!!#) = readWord32OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s U32#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → U32# → ST_# s
(¡#) = writeWord32OffAddr#
write# ∷ I {- ^ Offset in elements -} → U32# → Ref → ST_# s
write# i x r = (r ¡# i) x
