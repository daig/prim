module Ref.Ref.Stable where
import qualified Ref
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → Ref.Stable a
index# = indexStablePtrOffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s (Ref.Stable a)
read# = readStablePtrOffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → Ref.Stable a → ST_ s
write# = writeStablePtrOffAddr#
