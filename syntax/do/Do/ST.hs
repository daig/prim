--------------------------------------------------------------------
-- | Description : Do operations for the primitive ST monad
--------------------------------------------------------------------
module Do.ST where
import Unsafe.Coerce
import qualified GHC.Types as GHC

-- | Unsafely linearly consume the state token
escape# ∷ (☸) → (##)
escape# = unsafeCoerce# \ s → (##)
(☸) ∷ ∀ {r} (o ∷ T r). ((☸) → o) → o
(☸) = unsafeCoerce# runRW#

nop ∷ ST s (##)
nop s = (# s, (##) #)


-- | Used for @QualifiedDo@
return ∷ Pure (a ∷ T ra) ⇒ a → ST s a
return = η
{-# inline return #-}

st ∷ ST_ s → ST s (##)
st f = \s → case f s of s' → (# s', (##) #)
{-# inline st #-}

class Pure (a ∷ T ra) where η ∷ a → ST s a
class (Do a c, Do b c) ⇒ Lift2 (a ∷ T ra) (b ∷ T rb) (c ∷ T rc) where
  η2 ∷ (a → b → c) → ST s a → ST s b → ST s c

#define INST_LIFT2(A,B,C) \
instance Lift2 (a ∷ K A) (b ∷ K B) (c ∷ K C) where {\
  η2 f sta stb = sta >>= \a → stb >>= \b → η (f a b)}

#define INST_PURE(A) \
instance Pure (a ∷ K (A)) where {\
  η x = \s → (# s , x #)}

  --η2 f sta stb = sta >>= \a → stb >>= \b → η (f a b)}


-- | Return the value computed by a state thread.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
class RunST (a ∷ T ra) where runST ∷ (forall s. ST s a) → a
#define INST_RUNST(A) \
instance RunST (a ∷ K (A)) where {\
  runST st = case runRW# st of (# _, a #) → a}


{-
class Run (a ∷ T ra) where run ∷ ST s a → a
#define INST_RUN(A) \
instance Run (a ∷ K (A)) where {\
  run io = go (io ☸) where \
    go ∷ (# (☸) , a #) → a; \
    go (# s , a #) = (\(##) a → a) (escape# s) a}
-}

class Pure b ⇒ Do (a ∷ T ra) (b ∷ T rb) where
  (>>=) ∷ ST s a → (a → ST s b) → ST s b
  η1 ∷ (a → b) → ST s a → ST s b
  (>>) ∷ ST s (##) → ST s a → ST s a
  (*>) ∷ ST_ s → ST s a → ST s a
#define INST_MONAD(A,B) \
instance Do (a ∷ K (A)) (b ∷ K (B)) where {\
  η1 f st = st >>= \a → return (f a); \
  (st >> sta) s = case st s of {(# s', _ #) → sta s'} ;\
  (st *> sta) s = sta (st s) ;\
  (st >>= f) s = go (st s) f where \
    go ∷ (# State# s , a #) → (a → ST s b) → (# State# s , b #); \
    go (# s' , a #) f = f a s'}


INST_PURE((##))
INST_PURE(())
INST_PURE(ByteArray#)
INST_PURE(I) 
INST_PURE(I8)
INST_PURE(I16)
INST_PURE(I32)
INST_PURE(I64)
INST_PURE(U)
INST_PURE(U8)
INST_PURE(U16)
INST_PURE(U32)
INST_PURE(U64)
INST_PURE(Addr#)
INST_PURE(F32)
INST_PURE(F64)

#define INSTS2_MONAD(Y) \
INST_MONAD(Y,(##)); \
INST_MONAD(Y,()); \
INST_MONAD(Y,ByteArray#); \
INST_MONAD(Y,I); \
INST_MONAD(Y,I8); \
INST_MONAD(Y,I16); \
INST_MONAD(Y,I32); \
INST_MONAD(Y,I64); \
INST_MONAD(Y,U); \
INST_MONAD(Y,U8); \
INST_MONAD(Y,U16); \
INST_MONAD(Y,U32); \
INST_MONAD(Y,U64); \
INST_MONAD(Y,Addr#); \
INST_MONAD(Y,F32); \
INST_MONAD(Y,F64)

INSTS2_MONAD((##))
INSTS2_MONAD(())
INSTS2_MONAD(ByteArray#)
INSTS2_MONAD(I)
INSTS2_MONAD(I8)
INSTS2_MONAD(I16)
INSTS2_MONAD(I32)
INSTS2_MONAD(I64)
INSTS2_MONAD(U)
INSTS2_MONAD(U8)
INSTS2_MONAD(U16)
INSTS2_MONAD(U32)
INSTS2_MONAD(U64)
INSTS2_MONAD(Addr#)
INSTS2_MONAD(F32)
INSTS2_MONAD(F64)

#define INSTS3_LIFT(Y,Z) \
INST_LIFT2(Y,Z,(##)); \
INST_LIFT2(Y,Z,()); \
INST_LIFT2(Y,Z,ByteArray#); \
INST_LIFT2(Y,Z,I); \
INST_LIFT2(Y,Z,I8); \
INST_LIFT2(Y,Z,I16); \
INST_LIFT2(Y,Z,I32); \
INST_LIFT2(Y,Z,I64); \
INST_LIFT2(Y,Z,U); \
INST_LIFT2(Y,Z,U8); \
INST_LIFT2(Y,Z,U16); \
INST_LIFT2(Y,Z,U32); \
INST_LIFT2(Y,Z,U64); \
INST_LIFT2(Y,Z,Addr#); \
INST_LIFT2(Y,Z,F32); \
INST_LIFT2(Y,Z,F64)

#define INSTS2_LIFT(Y) \
INSTS3_LIFT(Y,(##)); \
INSTS3_LIFT(Y,()); \
INSTS3_LIFT(Y,ByteArray#); \
INSTS3_LIFT(Y,I); \
INSTS3_LIFT(Y,I8); \
INSTS3_LIFT(Y,I16); \
INSTS3_LIFT(Y,I32); \
INSTS3_LIFT(Y,I64); \
INSTS3_LIFT(Y,U); \
INSTS3_LIFT(Y,U8); \
INSTS3_LIFT(Y,U16); \
INSTS3_LIFT(Y,U32); \
INSTS3_LIFT(Y,U64); \
INSTS3_LIFT(Y,Addr#); \
INSTS3_LIFT(Y,F32); \
INSTS3_LIFT(Y,F64)

INSTS2_LIFT((##))
INSTS2_LIFT(())
INSTS2_LIFT(ByteArray#)
INSTS2_LIFT(I)
INSTS2_LIFT(I8)
INSTS2_LIFT(I16)
INSTS2_LIFT(I32)
INSTS2_LIFT(I64)
INSTS2_LIFT(U)
INSTS2_LIFT(U8)
INSTS2_LIFT(U16)
INSTS2_LIFT(U32)
INSTS2_LIFT(U64)
INSTS2_LIFT(Addr#)
INSTS2_LIFT(F32)
INSTS2_LIFT(F64)

INST_RUNST((##))
INST_RUNST(())
INST_RUNST(ByteArray#)
INST_RUNST(I)
INST_RUNST(I8)
INST_RUNST(I16)
INST_RUNST(I32)
INST_RUNST(I64)
INST_RUNST(U)
INST_RUNST(U8)
INST_RUNST(U16)
INST_RUNST(U32)
INST_RUNST(U64)
INST_RUNST(Addr#)
INST_RUNST(F32)
INST_RUNST(F64)
