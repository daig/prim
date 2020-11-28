--------------------------------------------------------------------
-- | Description : ByteCode operations for interpreters (GHCI)
--------------------------------------------------------------------
{-# language ScopedTypeVariables,TypeApplications #-}
module RTS.ByteCode where
import Bytes (A)
import qualified A.Boxed as Boxed

type BCO = BCO#

-- | Wrape a @BCO@ in a @AP_UPD@ thunk which will be updated with the value of
-- the @BCO@ when evaluated.
mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ A -- ^ instructions
    → A -- ^ literals
    → Boxed.A a -- ^ pointers
    → I -- ^ arity
    → A -- ^ static reference table usage bitmap
    → ST# s BCO
new = newBCO#

getApStackVal ∷ ∀ a b. a → I {- ^ stack depth -} → Maybe# b {- ^ The AP_STACK, if found -}
getApStackVal a i = coerce do getApStackVal# @_ @b a i
