module Array.Array.Byte where
import qualified Array
import Array.Array

index# :: A -> I64 -> Array.Byte
index# = indexByteArrayArray#

read# :: M s -> I64 -> ST s Array.Byte
read# = readByteArrayArray#

write# :: M s -> I64 -> Array.Byte -> ST_ s
write# = writeByteArrayArray#
