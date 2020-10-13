module Ref.U16 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → U16#
(!#) = indexWord16OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → U16#
index# i r = r !# i


(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST s U16#
(!!#) = readWord16OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST s U16#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → U16# → ST_ s
(¡#) = writeWord16OffAddr#
write# ∷ I {- ^ Offset in elements -} → U16# → Ref → ST_ s
write# i x r = (r ¡# i) x
