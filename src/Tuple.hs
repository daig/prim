--------------------------------------------------------------------
-- | Description : Unboxed Tuples
-- [Unboxed Tuples](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-tuples), written @(# a , b , c #)@ etc. and their constructors
-- @(# ,,, #)@ etc.
--
-- The maximum tuple size is 62 (even though some SIMD prim-ops use 64, there's no
-- way to construct them - this is a GHC bug.
--
-- The unit tuple is written @(# #)@, and is erased at runtime.
--
-- Unboxed Tuples are wired into GHC - this module is only needed for 'Void#',
-- which should only be used for compatibility.
--
-- 'Void#' (not to be confused with 'Void') is identical to the empty tuple
-- but exists for legacy reasons.
--
--------------------------------------------------------------------
module Tuple (Void#,void#) where
