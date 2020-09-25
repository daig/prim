module Array.Byte.Unpinned where
import Array.Byte

resize ∷ M s → I64 → ST s (M s)
resize = resizeMutableByteArray#
