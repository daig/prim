--------------------------------------------------------------------
-- | Description : Null-terminated C-like Strings
--
-- There's primitive syntax for these:
--
-- @"foo"#@ will allocate a fresh c-string
--
-- They are not aliased, in general @"foo"\# ≠ "foo"\#@
--------------------------------------------------------------------
module String.C (type S#) where

