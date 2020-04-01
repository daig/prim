module Array.Byte.Unpinned where
import Array.Byte

resize :: Mutable s -> I64 -> ST s (Mutable s)
resize = resizeMutableByteArray#
