--------------------------------------------------------------------
-- | Description : ByteCode operations for interpreters (GHCI)
--------------------------------------------------------------------
module RTS.ByteCode (type BCO, module RTS.ByteCode) where
import Coerce

-- | Wrap a @BCO@ in a @AP_UPD@ thunk which will be updated with the value of
-- the @BCO@ when evaluated.
mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ ∀ a s. A_ U8 -- ^ instructions
    → A_ U8 -- ^ literals
    → AR_ a -- ^ pointers
    → I -- ^ arity
    → A_ U8 -- ^ static reference table usage bitmap
    → ST s BCO
new = coerce (newBCO# @a)

getApStackVal ∷ ∀ a b. a → I {- ^ stack depth -} → (# (##) | b #) {- ^ The AP_STACK, if found -}
getApStackVal a i = case getApStackVal# a i of (# t, x #) → unsafeCoerce# (# t +# 1#, x #)
