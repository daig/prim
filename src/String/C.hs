--------------------------------------------------------------------
-- | Description : Null-terminated C-like Strings
--
-- There's primitive syntax for these:
--
-- @"foo"#@ will allocate a fresh c-string
--
-- They are not aliased, in general @"foo"\# ≠ "foo"\#@
--------------------------------------------------------------------
module String.C (type S, Stock.Char.Char(..)) where
import GHC.CString
import Stock.Char

-- | Null-terminated C-like Strings
--
-- Not a newtype because it would interfere with literal syntax
type S = P#
