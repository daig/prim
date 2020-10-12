module BCO where
import qualified Array

type BCO = BCO#

mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ Array.Byte → Array.Byte → Array.Boxed a → I → Array.Byte → ST s BCO
new = newBCO#
