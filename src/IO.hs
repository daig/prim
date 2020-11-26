--------------------------------------------------------------------
-- | Description : Primitive side-effecting/stateful computations
--------------------------------------------------------------------
module IO (Token,ST#,ST_#,type (☸),IO#,IO_#,run, η, (⇉)) where
import GHC.Magic

run ∷ ∀ (r ∷ RuntimeRep) (o ∷ T_ r). (Token (☸) → o) → o
run = runRW#

η ∷ a → ST# s a
η a = \s → (# s , a #)

(⊛) ∷ ST# s (a → b) → ST# s a → ST# s b
stf ⊛ sta = stf ⇉ (\ f → sta ⇉ (\ a → η (f a)))

(⇉) ∷ ST# s a → (a → ST# s b) → ST# s b
(st ⇉ f) s = case st s of (# s' , a #) → f a s'
