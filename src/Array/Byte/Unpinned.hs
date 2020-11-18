module Array.Byte.Unpinned where
import Array.Byte

resize ∷ M s → I → ST# s (M s)
resize = resizeMutableByteArray#
