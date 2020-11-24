module Array.Char8 where
import Char8
import Array.Byte

index# ∷ A → I {- ^ offset in bytes -} → Char8
index# = coerce indexCharArray#

read# ∷ MA s → I → ST# s Char8
read# = coerce readCharArray#

write# ∷ MA s → I → Char8 → ST_# s
write# = coerce writeCharArray#
