--------------------------------------------------------------------
-- | Description : Null-terminated C-like Strings
--
-- There's primitive syntax for these:
--
-- @"foo"#@ will allocate a fresh c-string
--
-- They are not aliased, in general @"foo"\# ≠ "foo"\#@
--------------------------------------------------------------------
module String.C where
import GHC.Prim

-- | Null-terminated C-like Strings
--
-- Not a newtype because it would interfere with literal syntax
type S = Addr#