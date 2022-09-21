--------------------------------------------------------------------
-- | Description : Null-terminated C-like Strings
--
-- There's primitive syntax for these:
--
-- Latin1# @"foo"#@ will allocate a fresh c-string
--
-- They are not aliased, in general @"foo"\# ≠ "foo"\#@
--
-- Latin-1 (8-bit) encoding
--------------------------------------------------------------------
module String.C.Latin (type Latin1#, module String.C.Latin) where
import GHC.CString

-- | Ignore null-termination
unpack# ∷ ∀ a. Latin1# → I {- ^ bytes to unpack -} → [Char]
unpack# = coerce unpackNBytes#
{-# inline conlike unpack# #-}

-- | Unpack and fold bytes
foldr ∷ ∀ a. Latin1# → (Char → a → a) → a → a
foldr = coerce (unpackFoldrCString# @a)
{-# inline conlike foldr #-}

len ∷ Latin1# → I; {-# inline conlike len #-}
len = cstringLength#
