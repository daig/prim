module Stock.B (module Stock.B, module X) where
import qualified Prelude as Prim
import GHC.Types (Bool(..),isTrue#)
import GHC.Classes as X ((&&),(||),not)

type B = Bool
pattern O ∷ B
pattern O = False
pattern I ∷ B
pattern I = True
{-# complete O,I #-}

pattern B# ∷ Prim.B → B
pattern B# i ← (Prim.dataToTag# → i) where B# i = isTrue# i

infixr 3 `and`
infixr 2 `or`
and, or ∷ B → B → B
and = (&&); {-# inline and #-}
or = (||); {-# inline or #-}
