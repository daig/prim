module Ref.U64 where
import Ref.Byte

(!#) ∷ Ref → I {- ^ Offset in elments -} → U64
(!#) = indexWord64OffAddr#
index# ∷ I {- ^ Offset in elments -} → Ref → U64
index# i r = r !# i

(!!#) ∷ Ref → I {- ^ Offset in elements -} → ST# s U64
(!!#) = readWord64OffAddr#
read# ∷ I {- ^ Offset in elments -} → Ref → ST# s U64
read# i r = r !!# i

(¡#) ∷ Ref → I {- ^ Offset in elements -} → U64 → ST_# s
(¡#) = writeWord64OffAddr#
write# ∷ I {- ^ Offset in elements -} → U64 → Ref → ST_# s
write# i x r = (r ¡# i) x
