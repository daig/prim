{-# language PartialTypeSignatures, LinearTypes #-}
module IO (module IO, module X) where
import ST as X


(☸) ∷ ∀ {r} (o ∷ T_ r). ((☸) ⊸ o) ⊸ o
(☸) = unsafeCoerce# runRW#

escape# ∷ (☸) ⊸ (##)
escape# = unsafeCoerce# \ s → (##)

run ∷ ∀ a. IO a ⊸ a
run io = go (io ☸) where
  go ∷ (# (☸) , a #) ⊸ a
  go (# s , a #) = (\(##) a → a) (escape# s) a
