module Error where
import Stock.Char

error ∷ ∀ r. ∀ (a ∷ T_ r). [Char] → a
error s = raise# (?callStack)
