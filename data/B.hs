module B (B(B#,I1,F,T),module X) where
import {-# source #-} I as X (I)
import {-# source #-} U as X (U)
import Bits as X
import Num as X
import Cast as X
import Cmp as X

newtype B ∷ T_I where B# ∷ {unB ∷ I} → B
