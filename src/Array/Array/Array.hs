module Array.Array.Array where
import qualified Array
import Array.Array

index# :: A -> I64 -> Array.Unlifted
index# = indexArrayArrayArray#

read# :: M s -> I64 -> ST s Array.Unlifted
read# = readArrayArrayArray#

write# :: M s -> I64 -> Array.Unlifted -> ST_ s
write# = writeArrayArrayArray#
