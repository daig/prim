module F64 where
import GHC.Types qualified as GHC

newtype F64 = F64# Double

(≡) ∷ F64 → F64 → B
(≡) = coerce eqDouble
{-# inline (≡) #-}
