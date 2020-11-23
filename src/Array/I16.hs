-- | Description : Packed 'Array.Byte' Arrays of 'I16#'
module Array.I16 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → I16#
index# = indexInt16Array#

index## ∷ A → I {- ^ Offset in bytes -} → I16#
index## = indexWord8ArrayAsInt16#

read# ∷ MA s → I → ST# s I16#
read# = readInt16Array#

write# ∷ MA s → I → I16# → ST_# s
write# = writeInt16Array#
