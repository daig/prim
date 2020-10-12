module B (module B, not) where
import GHC.Types (Bool(..),isTrue#)
import GHC.Classes ((&&),(||),not)

type B = Bool
pattern F ∷ B
pattern F = False
pattern T ∷ B
pattern T = True
{-# complete F,T #-}

pattern B# ∷ B# → B
pattern B# i ← (dataToTag# → i) where B# i = isTrue# i

infixr 3 ∧
infixr 2 ∨
(∧), (∨), and, or ∷ B → B → B
and = (&&); {-# inline and #-}
(∧) = (&&); {-# inline (∧) #-}
or = (||); {-# inline or #-}
(∨) = (||); {-# inline (∨) #-}
