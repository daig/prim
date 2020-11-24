module Stock.B (B(F,T), module Stock.B) where
import GHC.Types (Bool(..))
import GHC.Classes as GHC ((&&),(||),not)

pattern F ∷ B
pattern F = False
pattern T ∷ B
pattern T = True
{-# complete F,T #-}

pattern B ∷ Prim.B → B
pattern B i ← (coerce dataToTag# → b) where B b = coerce isTrue# b
{-# complete B #-}

(∧), (∨), and, or ∷ B → B → B
infixr 3 ∧ ; infixr 2 ∨
(∧) = (&&); (∨) = (||)
and = (&&); or  = (||) 
{-# inline (∧) #-}; {-# inline (∨) #-}
{-# inline and #-}; {-# inline or #-}
not ∷ B → B
not = GHC.not
