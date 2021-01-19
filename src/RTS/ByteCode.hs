--------------------------------------------------------------------
-- | Description : ByteCode operations for interpreters (GHCI)
--------------------------------------------------------------------
{-# language ScopedTypeVariables,TypeApplications #-}
module RTS.ByteCode (type BCO, module RTS.ByteCode) where
import A.Prim
import qualified A.Boxed.Big as Big

-- | Wrape a @BCO@ in a @AP_UPD@ thunk which will be updated with the value of
-- the @BCO@ when evaluated.
mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ A U8 -- ^ instructions
    → A U8 -- ^ literals
    → Big.A a -- ^ pointers
    → I -- ^ arity
    → A U8 -- ^ static reference table usage bitmap
    → ST# s BCO
new (A# is) (A# ls) ps a (A# bmp) = newBCO# is ls ps a bmp

getApStackVal ∷ ∀ a b. a → I {- ^ stack depth -} → Maybe# b {- ^ The AP_STACK, if found -}
getApStackVal a i = coerce do getApStackVal# @_ @b a i
