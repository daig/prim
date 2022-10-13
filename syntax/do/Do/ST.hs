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
instance Pure (a ∷ A) where {pure x = \s → (# s , x #)}

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
instance Do2 (a ∷ A) (b ∷ B) where {\
  st >% f = st >>= \a → return (f a); \
  f %< st = st >>= \a → return (f a); \
  st >>= f = \s → case st s of {(# ss, a #) → f a ss};\
  f =<< st = \s → case st s of {(# ss, a #) → f a ss}}

#define INSTS2_MONAD(Y) \
INST_PURE(Y);\
instance RunST (a ∷ Y) where {;\
  interleaveIO# io = \ s -> let r = case io (noDuplicate# s) of {(# _, x #) -> x } in (# s, r #);\
  runST st = case runRW# st of (# _, a #) → a};\
instance Do (a ∷ Y) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss};\
  f *<< st = \s → case st s of {(# ss, a #) → f a ss}};\
INST_MONAD(Y,T0); \
INST_MONAD(Y,★); \
INST_MONAD(Y,T_A); \
INST_MONAD(Y,T_I); \
INST_MONAD(Y,T_I1); \
INST_MONAD(Y,T_I2); \
INST_MONAD(Y,T_I4); \
INST_MONAD(Y,T_I8); \
INST_MONAD(Y,T_U); \
INST_MONAD(Y,T_U1); \
INST_MONAD(Y,T_U2); \
INST_MONAD(Y,T_U4); \
INST_MONAD(Y,T_U8); \
INST_MONAD(Y,T_P); \
INST_MONAD(Y,T_F4); \
INST_MONAD(Y,T_F8)

{-
#define INST_DO_TUP(A)\
instance Do (a ∷ K ((# A, ★ #))) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss};\
  f *<< st = \s → case st s of {(# ss, a #) → f a ss}}

INST_DO_TUP(T0)
INST_DO_TUP(T_A)
INST_DO_TUP(T_I)
INST_DO_TUP(T_I1)
INST_DO_TUP(T_I2)
INST_DO_TUP(T_I4)
INST_DO_TUP(T_I8)
INST_DO_TUP(T_U)
INST_DO_TUP(T_U1)
INST_DO_TUP(T_U2)
INST_DO_TUP(T_U4)
INST_DO_TUP(T_U8)
INST_DO_TUP(T_P)
INST_DO_TUP(T_F4)
INST_DO_TUP(T_F8)
INST_DO_TUP(★)
-}


INSTS2_MONAD(T0)
INSTS2_MONAD(★)
INSTS2_MONAD(T_A)
INSTS2_MONAD(T_I)
INSTS2_MONAD(T_I1)
INSTS2_MONAD(T_I2)
INSTS2_MONAD(T_I4)
INSTS2_MONAD(T_I8)
INSTS2_MONAD(T_U)
INSTS2_MONAD(T_U1)
INSTS2_MONAD(T_U2)
INSTS2_MONAD(T_U4)
INSTS2_MONAD(T_U8)
INSTS2_MONAD(T_P)
INSTS2_MONAD(T_F4)
INSTS2_MONAD(T_F8)

#ifdef LIFT_INSTS
class (Do a c, Do b c) ⇒ Lift2 (a ∷ T ra) (b ∷ T rb) (c ∷ T rc) where
  lift2 ∷ (a → b → c) → ST s a → ST s b → ST s c

#define INST_LIFT2(A,B,C) \
instance Lift2 (a ∷ K A) (b ∷ K B) (c ∷ K C) where {\
  lift2 f sta stb = sta >>= \a → stb >>= \b → pure (f a b)}

#define INSTS3_LIFT(Y,Z) \
INST_LIFT2(Y,Z,T0); \
INST_LIFT2(Y,Z,★); \
INST_LIFT2(Y,Z,T_A); \
INST_LIFT2(Y,Z,T_I); \
INST_LIFT2(Y,Z,T_I1); \
INST_LIFT2(Y,Z,T_I2); \
INST_LIFT2(Y,Z,T_I4); \
INST_LIFT2(Y,Z,T_I8); \
INST_LIFT2(Y,Z,T_U); \
INST_LIFT2(Y,Z,T_U1); \
INST_LIFT2(Y,Z,T_U2); \
INST_LIFT2(Y,Z,T_U4); \
INST_LIFT2(Y,Z,T_U8); \
INST_LIFT2(Y,Z,T_P); \
INST_LIFT2(Y,Z,T_F4); \
INST_LIFT2(Y,Z,T_F8)

#define INSTS2_LIFT(Y) \
INSTS3_LIFT(Y,T0); \
INSTS3_LIFT(Y,★); \
INSTS3_LIFT(Y,T_A); \
INSTS3_LIFT(Y,T_I); \
INSTS3_LIFT(Y,T_I1); \
INSTS3_LIFT(Y,T_I2); \
INSTS3_LIFT(Y,T_I4); \
INSTS3_LIFT(Y,T_I8); \
INSTS3_LIFT(Y,T_U); \
INSTS3_LIFT(Y,T_U1); \
INSTS3_LIFT(Y,T_U2); \
INSTS3_LIFT(Y,T_U4); \
INSTS3_LIFT(Y,T_U8); \
INSTS3_LIFT(Y,T_P); \
INSTS3_LIFT(Y,T_F4); \
INSTS3_LIFT(Y,T_F8)

INSTS2_LIFT(T0)
INSTS2_LIFT(★)
INSTS2_LIFT(T_A)
INSTS2_LIFT(T_I)
INSTS2_LIFT(T_I1)
INSTS2_LIFT(T_I2)
INSTS2_LIFT(T_I4)
INSTS2_LIFT(T_I8)
INSTS2_LIFT(T_U)
INSTS2_LIFT(T_U1)
INSTS2_LIFT(T_U2)
INSTS2_LIFT(T_U4)
INSTS2_LIFT(T_U8)
INSTS2_LIFT(T_P)
INSTS2_LIFT(T_F4)
INSTS2_LIFT(T_F8)
#endif
