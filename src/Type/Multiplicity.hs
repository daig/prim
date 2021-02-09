--------------------------------------------------------------------
-- | Description : Type indexes for linear types
--
-- GHC Supports [@-XLinearTypes@](https://downloads.haskell.org/ghc/9.0.1-rc1/docs/html/users_guide/exts/linear_types.html)
-- Written as @a → b@, @a %1 → b@, or @a %One → b@.
-- Normal unrestricted functions are  written as @a → b@ or @a %Many → b@
--
-- When @-XLinearTypes@ is turned on, 
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Type.Multiplicity
  (Multiplicity(..)
  ,MultMul
  ,FUN
  ) where
import GHC.Types
import GHC.Prim
