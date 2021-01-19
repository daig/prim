{-# OPTIONS_HADDOCK not-home #-}
module Prelude (S,I,Char, module X) where
import GHC.Prim
import GHC.Types
import GHC.CString as X
-- | Null-terminated C-like Strings
--
-- Not a newtype because it would interfere with literal syntax
type S = Addr#
type I = Int#
