{-# language GHCForeignImportPrim,UnliftedFFITypes #-}
module IO.Exception.Arithmetic where
import GHC.Prim.Exception as GHC


foreign import prim "stg_raiseOverflowzh"  raiseOverflow  ∷ IO X#
foreign import prim "stg_raiseUnderflowzh" raiseUnderflow ∷ IO X#
foreign import prim "stg_raiseDivZZerozh"  raiseDivZero   ∷ IO X#

overflow#, underflow#, div0# ∷ a
overflow# = GHC.raiseOverflow
underflow# = GHC.raiseUnderflow
div0# = GHC.raiseDivZero
{-# inline overflow# #-};{-# inline underflow# #-};{-# inline div0# #-}
