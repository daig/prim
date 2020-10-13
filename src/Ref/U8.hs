module Ref.U8 where
import Ref.Byte
(!#) ∷ Ref → I {- ^ Offset in elments -} → U8#
(!#) = indexWord8OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → U8#
index# i r = r !# i


(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST s U8#
(!!#) = readWord8OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST s U8#
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → U8# → ST_ s
(¡#) = writeWord8OffAddr#
write# ∷ I {- ^ Offset in elements -} → U8# → Ref → ST_ s
write# i x r = (r ¡# i) x
