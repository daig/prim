module Classes
  (type (≡)(..),type (≤)(..),Ordering(Ordering#,LT,EQ,GE)
  ,𝔹(..),ℕ(..),ℤ(..),ℝ(..)
  ,Cast(..)
  ,module X
  ) where
import Types as X (T_,T_I)
import GHC.Prim as X (Int#,Word#)
import Types
import GHC.Prim
