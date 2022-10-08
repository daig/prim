--------------------------------------------------------------------
-- | Description : Do operations for the primitive ST monad
--------------------------------------------------------------------
module Do.ST (runRW#, module Do.ST) where
import Unsafe.Coerce
import qualified GHC.Types as GHC

-- | Used for @QualifiedDo@
return ∷ Pure (a ∷ T ra) ⇒ a → ST s a
return = pure; {-# inline return #-}

class Pure (a ∷ T ra) where pure ∷ a → ST s a

#define INST_PURE(A) \
instance Pure (a ∷ K (A)) where {pure x = \s → (# s , x #)}

-- | Return the value computed by a state thread.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
class RunST (a ∷ T ra) where runST ∷ (∀ s. ST s a) → a

class Pure b ⇒ Do2 (a ∷ T ra) (b ∷ T rb) where
  (>>=) ∷ ST s a → (a → ST s b) → ST s b
  (>%) ∷ ST s a → (a → b) → ST s b

class Do (a ∷ T ra) where
  (>>*) ∷ ST s a → (a → ST_ s) → ST_ s
  (>*) ∷ ST s a → ST_ s → ST_ s
  (>>) ∷ ST_ s → ST s a → ST s a

(<>) ∷ ST_ s → ST_ s → ST_ s
st <> sta = \s → sta (st s)

infixl 1 >>=, >>, >*, >>*, >%

#define INST_MONAD(A,B) \
instance Do2 (a ∷ K (A)) (b ∷ K (B)) where {\
  st >% f = st >>= \a → return (f a); \
  st >>= f = \s → case st s of {(# ss, a #) → f a ss}}

#define INSTS2_MONAD(Y) \
INST_PURE(Y);\
instance RunST (a ∷ K (Y)) where {runST st = case runRW# st of (# _, a #) → a};\
instance Do (a ∷ K (Y)) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss}};\
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

#ifdef LIFT_INSTS
class (Do a c, Do b c) ⇒ Lift2 (a ∷ T ra) (b ∷ T rb) (c ∷ T rc) where
  lift2 ∷ (a → b → c) → ST s a → ST s b → ST s c

#define INST_LIFT2(A,B,C) \
instance Lift2 (a ∷ K A) (b ∷ K B) (c ∷ K C) where {\
  lift2 f sta stb = sta >>= \a → stb >>= \b → pure (f a b)}

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
#endif
