--------------------------------------------------------------------
-- | Description : ByteCode operations for interpreters (GHCI)
--------------------------------------------------------------------
module RTS.ByteCode (type BCO, module RTS.ByteCode) where

-- | Wrap a @BCO@ in a @AP_UPD@ thunk which will be updated with the value of
-- the @BCO@ when evaluated.
mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ ∀ a s. A U8 -- ^ instructions
    → A U8 -- ^ literals
    → A a -- ^ pointers
    → I -- ^ arity
    → A U8 -- ^ static reference table usage bitmap
    → ST s BCO
new = coerce (newBCO# @a)

getApStackVal ∷ ∀ a b. a → I {- ^ stack depth -} → Maybe# b {- ^ The AP_STACK, if found -}
getApStackVal = coerce (getApStackVal# @a @b)
