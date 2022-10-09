--------------------------------------------------------------------
-- | Description : Stable identity of a haskell expression. Safe as a key
--------------------------------------------------------------------
module P.Stable.Name (P_Stable_Name, StableName#
                      -- * misc utilities
                      ,module P.Stable.Name
                      -- * instance reexports
                     ,module X
                     )where
import Cast as X (Cast(..))
import Cmp as X (Eq#(..))

new ∷ a → IO (P_Stable_Name a)
new = makeStableName#
