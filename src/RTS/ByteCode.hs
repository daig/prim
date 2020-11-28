--------------------------------------------------------------------
-- | Description : ByteCode operations for interpreters (GHCI)
--------------------------------------------------------------------
{-# language ScopedTypeVariables,TypeApplications #-}
module RTS.ByteCode where
import qualified A.Byte as Byte
import qualified A.Boxed.Big as Big

type BCO = BCO#

-- | Wrape a @BCO@ in a @AP_UPD@ thunk which will be updated with the value of
-- the @BCO@ when evaluated.
mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ Byte.A -- ^ instructions
    → Byte.A -- ^ literals
    → Big.A a -- ^ pointers
    → I -- ^ arity
    → Byte.A -- ^ static reference table usage bitmap
    → ST# s BCO
new = newBCO#

getApStackVal ∷ ∀ a b. a → I {- ^ stack depth -} → Maybe# b {- ^ The AP_STACK, if found -}
getApStackVal a i = coerce do getApStackVal# @_ @b a i
