module Array.Char where
import Array.Byte

index# :: A
       -> I64 -- Offset in 4-byte words
       -> Char
index# = indexWideCharArray#

index## :: A
        -> I64 -- Offset in bytes
        -> Char
index## = indexWord8ArrayAsWideChar#


read# :: M s -> I64 -> ST s Char
read# = readWideCharArray#

write# :: M s -> I64 -> Char -> ST_ s
write# = writeWideCharArray#
