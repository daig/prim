{-# language UndecidableSuperClasses, InstanceSigs, CPP #-}
module Array.Buffer where
import Prim
import Memset
import qualified GHC.Types as GHC
import Action
import Do.ST as ST
import Array
import Cast


  
type Buffer ∷ ∀ {rx} {r}. (T rx → T r) → TC
class Buffer a where ( # ) ∷ a x → I → a # x

infixl 9 #

instance Buffer A_ where ( # ) x i = Bytes'_Off# (# coerce x, i #)
instance Buffer A_# where ( # ) (Bytes'_Off# (# x, i #)) n = Bytes'_Off_Len# (# coerce x, i, n #)
instance Buffer P_ where ( # ) x i = P__Len# (# coerce x, i #)
instance Buffer (A s) where ( # ) x i = Bytes_Off# (# coerce x, i #)
instance Buffer (A# s) where ( # ) (Bytes_Off# (# x, i #)) n = Bytes_Off_Len# (# coerce x, i, n #)
instance Buffer (P s) where ( # ) x i = P_Len# (# coerce x, i #)
