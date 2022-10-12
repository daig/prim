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
class RunST (a ∷ T ra) where
  runST ∷ (∀ s. ST s a) → a
  interleaveIO# :: IO a -> IO a


class Pure b ⇒ Do2 (a ∷ T ra) (b ∷ T rb) where
  (>>=) ∷ ST s a → (a → ST s b) → ST s b
  (=<<) ∷ (a → ST s b) → ST s a → ST s b
  (>%) ∷ ST s a → (a → b) → ST s b
  (%<) ∷ (a → b) → ST s a → ST s b

class Do (a ∷ T ra) where
  (>>*) ∷ ST s a → (a → ST_ s) → ST_ s
  (*<<) ∷ (a → ST_ s) → ST s a → ST_ s
  (>*) ∷ ST s a → ST_ s → ST_ s
  (>>) ∷ ST_ s → ST s a → ST s a

(<>) ∷ ST_ s → ST_ s → ST_ s
st <> sta = \s → sta (st s)

infixl 1 >>=, >>, >*, >>*, >%, <>
infixr 1 =<<, *<<, %<

#define INST_MONAD(A,B) \
instance Do2 (a ∷ K (A)) (b ∷ K (B)) where {\
  st >% f = st >>= \a → return (f a); \
  f %< st = st >>= \a → return (f a); \
  st >>= f = \s → case st s of {(# ss, a #) → f a ss};\
  f =<< st = \s → case st s of {(# ss, a #) → f a ss}}

#define INSTS2_MONAD(Y) \
INST_PURE(Y);\
instance RunST (a ∷ K (Y)) where {;\
  interleaveIO# io = \ s -> let r = case io (noDuplicate# s) of {(# _, x #) -> x } in (# s, r #);\
  runST st = case runRW# st of (# _, a #) → a};\
instance Do (a ∷ K (Y)) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss};\
  f *<< st = \s → case st s of {(# ss, a #) → f a ss}};\
INST_MONAD(Y,(##)); \
INST_MONAD(Y,()); \
INST_MONAD(Y,ByteArray#); \
INST_MONAD(Y,I); \
INST_MONAD(Y,I1); \
INST_MONAD(Y,I2); \
INST_MONAD(Y,I4); \
INST_MONAD(Y,I8); \
INST_MONAD(Y,U); \
INST_MONAD(Y,U1); \
INST_MONAD(Y,U2); \
INST_MONAD(Y,U4); \
INST_MONAD(Y,U8); \
INST_MONAD(Y,Addr#); \
INST_MONAD(Y,F4); \
INST_MONAD(Y,F8)

#define INST_DO_TUP(A)\
instance Do (a ∷ K ((# A, () #))) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss};\
  f *<< st = \s → case st s of {(# ss, a #) → f a ss}}

INST_DO_TUP((##))
INST_DO_TUP(ByteArray#)
INST_DO_TUP(I)
INST_DO_TUP(I1)
INST_DO_TUP(I2)
INST_DO_TUP(I4)
INST_DO_TUP(I8)
INST_DO_TUP(U)
INST_DO_TUP(U1)
INST_DO_TUP(U2)
INST_DO_TUP(U4)
INST_DO_TUP(U8)
INST_DO_TUP(Addr#)
INST_DO_TUP(F4)
INST_DO_TUP(F8)
INST_DO_TUP(())



INSTS2_MONAD((##))
INSTS2_MONAD(())
INSTS2_MONAD(ByteArray#)
INSTS2_MONAD(I)
INSTS2_MONAD(I1)
INSTS2_MONAD(I2)
INSTS2_MONAD(I4)
INSTS2_MONAD(I8)
INSTS2_MONAD(U)
INSTS2_MONAD(U1)
INSTS2_MONAD(U2)
INSTS2_MONAD(U4)
INSTS2_MONAD(U8)
INSTS2_MONAD(Addr#)
INSTS2_MONAD(F4)
INSTS2_MONAD(F8)

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
INST_LIFT2(Y,Z,I1); \
INST_LIFT2(Y,Z,I2); \
INST_LIFT2(Y,Z,I4); \
INST_LIFT2(Y,Z,I8); \
INST_LIFT2(Y,Z,U); \
INST_LIFT2(Y,Z,U1); \
INST_LIFT2(Y,Z,U2); \
INST_LIFT2(Y,Z,U4); \
INST_LIFT2(Y,Z,U8); \
INST_LIFT2(Y,Z,Addr#); \
INST_LIFT2(Y,Z,F4); \
INST_LIFT2(Y,Z,F8)

#define INSTS2_LIFT(Y) \
INSTS3_LIFT(Y,(##)); \
INSTS3_LIFT(Y,()); \
INSTS3_LIFT(Y,ByteArray#); \
INSTS3_LIFT(Y,I); \
INSTS3_LIFT(Y,I1); \
INSTS3_LIFT(Y,I2); \
INSTS3_LIFT(Y,I4); \
INSTS3_LIFT(Y,I8); \
INSTS3_LIFT(Y,U); \
INSTS3_LIFT(Y,U1); \
INSTS3_LIFT(Y,U2); \
INSTS3_LIFT(Y,U4); \
INSTS3_LIFT(Y,U8); \
INSTS3_LIFT(Y,Addr#); \
INSTS3_LIFT(Y,F4); \
INSTS3_LIFT(Y,F8)

INSTS2_LIFT((##))
INSTS2_LIFT(())
INSTS2_LIFT(ByteArray#)
INSTS2_LIFT(I)
INSTS2_LIFT(I1)
INSTS2_LIFT(I2)
INSTS2_LIFT(I4)
INSTS2_LIFT(I8)
INSTS2_LIFT(U)
INSTS2_LIFT(U1)
INSTS2_LIFT(U2)
INSTS2_LIFT(U4)
INSTS2_LIFT(U8)
INSTS2_LIFT(Addr#)
INSTS2_LIFT(F4)
INSTS2_LIFT(F8)
#endif
