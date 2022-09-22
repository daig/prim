module B (B,module X) where
import GHC.Types as X (Bool)
import GHC.Prim as X (dataToTag#,tagToEnum#)

type B = Bool

(∧), (∨) ∷ B → B → B
(∧) = coerce (&&)
(∨) = coerce (||)
{-# inline (∧) #-}
{-# inline (∨) #-}
(¬) ∷ B → B
(¬) = coerce not
{-# inline (¬) #-}

--pattern F = False
--pattern T = True
--[># complete F , T #<]
--pattern B ∷ Ξ B → B
--pattern B t ← ((\b → B (dataToTag# b)) → t) where B (B b) = tagToEnum# b
