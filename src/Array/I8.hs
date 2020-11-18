-- | Description : Packed 'Array.Byte' Arrays of 'I8#'
module Array.I8 where
import Array.Byte

index# ∷ A → I {- ^ Offset in elments -} → I8#
index# = indexInt8Array#

read# ∷ M s → I → ST# s I8#
read# = readInt8Array#

write# ∷ M s → I → I8# → ST_# s
write# = writeInt8Array#
