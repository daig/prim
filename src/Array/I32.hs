-- | Description : Packed 'Array.Byte' Arrays of 'I32#'
module Array.I32 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → I32#
index# = indexInt32Array#

index## ∷ A → I {- ^ Offset in bytes -} → I32#
index## = indexWord8ArrayAsInt32#

read# ∷ M s → I → ST s I32#
read# = readInt32Array#

write# ∷ M s → I → I32# → ST_ s
write# = writeInt32Array#
