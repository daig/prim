--------------------------------------------------------------------
-- | Description : Latin-1 (8-bit) encoding
--------------------------------------------------------------------
module String.C.Latin (type S, module String.C.Latin) where
import GHC.CString

-- | Unpack bytes until \null byte
unpack# ∷ S → [Char]
unpack# = unpackCString#
{-# inline conlike unpack# #-}

-- | Ignore null-termination
unpackN# ∷ S → I → [Char]
unpackN# = unpackNBytes#
{-# inline conlike unpackN# #-}

-- | Unpack bytes and append
(∔) ∷ S → [Char] → [Char]
(∔) = unpackAppendCString#
{-# inline conlike (∔) #-}

-- | Unpack and fold bytes
foldr ∷ S → (Char → a → a) → a → a
foldr = unpackFoldrCString#
{-# inline conlike foldr #-}
