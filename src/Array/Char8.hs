module Array.Char8 where
import Array.Byte

index# ∷ A → I {- ^ offset in bytes -} → Char8#
index# = indexCharArray#

read# ∷ MA s → I → ST# s Char8#
read# = readCharArray#

write# ∷ MA s → I → Char8# → ST_# s
write# = writeCharArray#
