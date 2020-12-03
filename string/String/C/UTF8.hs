--------------------------------------------------------------------
-- | Description : UTF-8 encoding
--------------------------------------------------------------------
module String.C.UTF8 (type S, unpack#) where
import GHC.CString

-- | Unpack UTF-8 encoded 'Char' string
unpack# ∷ S → [Char]
unpack# = unpackCStringUtf8#
{-# inline conlike unpack# #-}
