module Array.Byte.Unpinned where
import Array.Byte

resize ∷ MA s → I → ST# s (MA s)
resize = resizeMutableByteArray#
