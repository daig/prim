module BCO where
import qualified Array.Byte as Byte
import qualified Array.Boxed as Boxed

type BCO = BCO#

mkApUpd0 ∷ BCO → (# a #)
mkApUpd0 = mkApUpd0#

new ∷ Byte.A → Byte.A → Boxed.A a → I → Byte.A → ST# s BCO
new = newBCO#
