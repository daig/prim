--------------------------------------------------------------------
-- | Description : Monad operations for ST
--------------------------------------------------------------------
{-# language LinearTypes #-}
module ST.Do (module ST.Do, module X) where
import ST as X
import {-# source #-} I
import {-# source #-} I8
import {-# source #-} I16
import {-# source #-} I32
import {-# source #-} P
import {-# source #-} A
import I64
import U
import U8
import U16
import U32
import U64
import {-# source #-} IO

(⊕) ∷ I ⊸ I ⊸ I
(⊕) = unsafeCoerce# (+#)

return ∷ Pure (s ∷ T_ rs) (a ∷ T_ ra) ⇒ a ⊸ ST s a
return = η
(>>=) ∷ Monad (s ∷ T_ rs) (a ∷ T_ ra) (b ∷ T_ rb)
  ⇒ ST s a ⊸ (a ⊸ ST s b) ⊸ ST s b
(>>=) = (⇉)
{-# inline (>>=) #-}
{-# inline return #-}

class Pure (s ∷ T_ rs) (a ∷ T_ ra) where
  η ∷ a ⊸ ST s a
  η2 ∷ (a ⊸ a ⊸ a) ⊸ ST s a ⊸ ST s a ⊸ ST s a
#define INST_PURE(S,A) \
instance Pure (S) (A) where {\
  η x = \s → (# s , x #); \
  η2 f sta stb = sta ⇉ \a → stb ⇉ \b → return (f a b)}

class RunIO (a ∷ T_ ra) where runIO ∷ IO a ⊸ a
#define INST_RUNIO(A) \
instance RunIO A where {\
  runIO io = go (io ☸) where \
    go ∷ (# (☸) , A #) ⊸ A; \
    go (# s , a #) = (\(##) a → a) (escape# s) a}

class Monad (s ∷ T_ rs) (a ∷ T_ ra) (b ∷ T_ rb) where
  (⇉) ∷ ST s a ⊸ (a ⊸ ST s b) ⊸ ST s b
  η1 ∷ (a ⊸ b) ⊸ ST s a ⊸ ST s b
  (*>) ∷ ST_ s ⊸ ST s a ⊸ ST s a
#define INST_MONAD(S,A,B) \
instance Monad (S) (A) (B) where {\
  η1 f st = st ⇉ \a → return (f a); \
  (st *> sta) s = sta (st s); \
  (st ⇉ f) s = go (st s) f where \
    go ∷ (# S , A #) ⊸ (A ⊸ ST S B) ⊸ (# S , B #); \
    go (# s' , a #) f = f a s'}

INST_RUNIO(a)
INST_RUNIO(I)
INST_RUNIO(I8)
INST_RUNIO(I16)
INST_RUNIO(I32)
INST_RUNIO(I64)
INST_RUNIO(U)
INST_RUNIO(U8)
INST_RUNIO(U16)
INST_RUNIO(U32)
INST_RUNIO(U64)

#define INSTS_PURE(S) \
INST_PURE(S,a); \
INST_PURE(S,I); \
INST_PURE(S,I8); \
INST_PURE(S,I16); \
INST_PURE(S,I32); \
INST_PURE(S,I64); \
INST_PURE(S,U); \
INST_PURE(S,U8); \
INST_PURE(S,U16); \
INST_PURE(S,U32); \
INST_PURE(S,U64)

INSTS_PURE(s)
INSTS_PURE((☸))
INSTS_PURE(P#)
INSTS_PURE(A##)

#define INSTS_A_MONAD(S,A) \
INST_MONAD(S,A,b); \
INST_MONAD(S,A,I); \
INST_MONAD(S,A,I8); \
INST_MONAD(S,A,I16); \
INST_MONAD(S,A,I32); \
INST_MONAD(S,A,I64); \
INST_MONAD(S,A,U); \
INST_MONAD(S,A,U8); \
INST_MONAD(S,A,U16); \
INST_MONAD(S,A,U32); \
INST_MONAD(S,A,U64)

#define INSTS_MONAD(s) \
INSTS_A_MONAD(s,a); \
INSTS_A_MONAD(s,I); \
INSTS_A_MONAD(s,I8); \
INSTS_A_MONAD(s,I16); \
INSTS_A_MONAD(s,I32); \
INSTS_A_MONAD(s,I64); \
INSTS_A_MONAD(s,U); \
INSTS_A_MONAD(s,U8); \
INSTS_A_MONAD(s,U16); \
INSTS_A_MONAD(s,U32); \
INSTS_A_MONAD(s,U64)

INSTS_MONAD(s)
INSTS_MONAD((☸))
INSTS_MONAD(P#)
INSTS_MONAD(A##)
