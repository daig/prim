module Array.Char8 where
import Array.Byte

index# ∷ A → I {- ^ offset in bytes -} → Char8#
index# = indexCharArray#

read# ∷ M s → I → ST s Char8#
read# = readCharArray#

write# ∷ M s → I → Char8# → ST_ s
write# = writeCharArray#
