module P.Stable.Name where
import Prelude hiding (P)

new ∷ a → IO (P_Stable_Name a)
new = makeStableName#
