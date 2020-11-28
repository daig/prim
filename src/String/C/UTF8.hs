--------------------------------------------------------------------
-- | Description : UTF-8 encoding
--------------------------------------------------------------------
module String.C.UTF8 (module X, unpack#) where
import GHC.CString
import String.C as X
import Prelude hiding (Char)

-- | Unpack UTF-8 encoded 'Char' string
unpack# ∷ S → [Char]
unpack# = unpackCStringUtf8#
{-# inline conlike unpack# #-}
