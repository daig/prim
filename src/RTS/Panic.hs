--------------------------------------------------------------------
-- | Description : Uncatchable errors
--------------------------------------------------------------------
{-# language GHCForeignImportPrim,UnliftedFFITypes #-}
module RTS.Panic (module RTS.Panic, module X) where
import Prim.IO as X (IO)
import Prim.IO
import String.C as X (S)
import Prim.Void as X (Void)
import Prim.Void

-- | Display the string and exit
foreign import prim "stg_paniczh" panic ∷ S → IO Void

-- | Display the string and exit
panic# ∷ S → a
panic# e = run \ s → case panic e s of (# _, v #) → absurd v
