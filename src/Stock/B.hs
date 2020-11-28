{-# language TypeApplications,NoImplicitPrelude #-}
module Stock.B (B, module Stock.B) where
import GHC.Prim (dataToTag#)
import GHC.Types (Bool(..),isTrue#)
import GHC.Classes as GHC ((&&),(||),not)
import qualified B as Prim
import GHC.Coerce
import Bits
import Stock.Eq

type B = Bool

pattern F ∷ B
pattern F = False
pattern T ∷ B
pattern T = True
{-# complete F,T #-}

pattern B ∷ Prim.B → B
pattern B b ← (coerce (dataToTag# @B) → b) where B b = coerce isTrue# b
{-# complete B #-}

instance 𝔹 B where (∧) = (&&); (∨) = (||); (⊕) = (/=); (¬) = GHC.not
