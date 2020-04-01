module Array.Char8 where
import Array.Byte

index# :: A
       -> I64 -- ^ offset in bytes
       -> Char8#
index# = indexCharArray#

read# :: M s -> I64 -> ST s Char8#
read# = readCharArray#

write# :: M s -> I64 -> Char8# -> ST_ s
write# = writeCharArray#
