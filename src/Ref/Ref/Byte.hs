module Ref.Ref.Byte where
import qualified Ref
import Ref.Byte

index# ∷ Ref → I64 {- ^ Offset in elments -} → Ref.Byte
index# = indexAddrOffAddr#

read# ∷ Ref → I64 {- ^ Offset in elements -} → ST s Ref.Byte
read# = readAddrOffAddr#

write# ∷ Ref → I64 {- ^ Offset in elements -} → Ref.Byte → ST_ s
write# = writeAddrOffAddr#
