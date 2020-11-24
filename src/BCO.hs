module BCO where
import A (A)
import qualified A.Boxed as Boxed

type BCO = BCO#

mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ A → A → Boxed.A a → I → A → ST# s BCO
new = newBCO#
