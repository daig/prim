module A.Unpinned where
import A

resize ∷ MA s → I → ST# s (MA s)
resize = resizeMutableByteArray#
