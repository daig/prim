module Array.Char where
import Array.Byte

index# :: Array
       -> I64 -- Offset in 4-byte words
       -> Char
index# = indexWideCharArray#

index## :: Array
        -> I64 -- Offset in bytes
        -> Char
index## = indexWord8ArrayAsWideChar#


read# :: Mutable s -> I64 -> ST s Char
read# = readWideCharArray#

write# :: Mutable s -> I64 -> Char -> ST_ s
write# = writeWideCharArray#
