{-# language UndecidableSuperClasses, InstanceSigs, CPP #-}
module Array.Buffer where
import Prim
import Memset
import qualified GHC.Types as GHC
import Action
import Do.ST as ST
import Array
import Cast


type family OffRep (r ∷ RuntimeRep) = (rr ∷ RuntimeRep) | rr → r where
  OffRep AddrRep = TupleRep '[AddrRep, IntRep]
  OffRep (BoxedRep Unlifted) = TupleRep '[BoxedRep Unlifted, IntRep]
  OffRep (TupleRep '[BoxedRep Unlifted, IntRep]) = TupleRep '[BoxedRep Unlifted, IntRep, IntRep]
  
-- | The type of a reference with extra context
type Off ∷ ∀ {rx} {r}. (T rx → T r) → T rx → T (OffRep r)
type family Off a = aa | aa → a where
  Off UnboxedArray# = UnboxedConstRef
  Off UnboxedConstRef = UnboxedSlice
  Off ForeignArray# = ForeignSlice
  Off (UnboxedMutableArray# s) = UnboxedRef s
  Off (UnboxedRef s) = UnboxedMutableSlice s
  Off (ForeignMutableArray# s) = ForeignMutableSlice s

type Buffer ∷ ∀ {rx} {r}. (T rx → T r) → TC
class Buffer a where ( # ) ∷ a x → I → Off a x

infixl 9 #

instance Buffer UnboxedArray# where ( # ) x i = Bytes_Off# (# coerce x, i #)
instance Buffer UnboxedConstRef where ( # ) (Bytes_Off# (# x, i #)) n = Bytes_Off_Len# (# coerce x, i, n #)
instance Buffer ForeignArray# where ( # ) x i = Addr_Len# (# coerce x, i #)
instance Buffer (UnboxedMutableArray# s) where ( # ) x i = MBytes_Off# (# coerce x, i #)
instance Buffer (UnboxedRef s) where ( # ) (MBytes_Off# (# x, i #)) n = MBytes_Off_Len# (# coerce x, i, n #)
instance Buffer (ForeignMutableArray# s) where ( # ) x i = MAddr_Len# (# coerce x, i #)
