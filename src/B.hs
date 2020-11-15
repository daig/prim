module B (B, B#, module B) where
import GHC.Types (Bool(..),isTrue#)
import GHC.Classes as GHC ((&&),(||),not)

pattern F ∷ B
pattern F = False
pattern T ∷ B
pattern T = True
{-# complete F,T #-}

infixl 3 ∧#
infixl 2 ∨#
(∧#), (∨#) ∷ B# → B# → B#
(∧#) = andI#; (∨#) = orI#;


pattern B# ∷ B# → B
pattern B# i ← (dataToTag# → i) where B# i = isTrue# i

(∧), (∨), and, or ∷ B → B → B
infixr 3 ∧ ; infixr 2 ∨
(∧) = (&&); (∨) = (||)
and = (&&); or  = (||) 
{-# inline (∧) #-}; {-# inline (∨) #-}
{-# inline and #-}; {-# inline or #-}
not ∷ B → B
not = GHC.not
