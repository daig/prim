--------------------------------------------------------------------
-- | Description : Uncatchable errors
--------------------------------------------------------------------
{-# language GHCForeignImportPrim,UnliftedFFITypes #-}
module Panic where
import IO
import String.C

-- | Display the string and exit
foreign import prim "stg_paniczh" panic ∷ S → IO# Void

-- | Display the string and exit
panic# ∷ S → a
panic# e = run \ s → case panic e s of (# _, v #) → absurd v
