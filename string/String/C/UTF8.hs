--------------------------------------------------------------------
-- | Description : Null-terminated C-like Strings
--
-- There's primitive syntax for these:
--
-- Latin1# @"foo"#@ will allocate a fresh c-string
--
-- They are not aliased, in general @"foo"\# ≠ "foo"\#@
--
-- UTF8 encoding
--------------------------------------------------------------------
module String.C.UTF8 (type UTF8#, foldr) where
import GHC.CString

-- | Unpack and fold 'Char's
foldr ∷ ∀ a. UTF8# → (Char → a → a) → a → a
foldr = coerce (unpackFoldrCStringUtf8# @a)
{-# inline conlike foldr #-}
