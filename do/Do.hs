--------------------------------------------------------------------
-- | Description : Monad operations for the primitive ST monad
--------------------------------------------------------------------
module Do where
import Unsafe.Coerce
import qualified GHC.Types as GHC

-- | Unsafely linearly consume the state token
escape# ∷ (☸) ⊸ (##)
escape# = unsafeCoerce# \ s → (##)
(☸) ∷ ∀ {r} (o ∷ T r). ((☸) ⊸ o) ⊸ o
(☸) = unsafeCoerce# runRW#


-- | Used for @QualifiedDo@
return ∷ Pure (s ∷ T rs) (a ∷ T ra) ⇒ a ⊸ ST s a
return = η
-- | Used for @QualifiedDo@
(>>=) ∷ Monad (s ∷ T rs) (a ∷ T ra) (b ∷ T rb)
  ⇒ ST s a ⊸ (a ⊸ ST s b) ⊸ ST s b
(>>=) = (⇉)
{-# inline (>>=) #-}
{-# inline return #-}

class Pure (s ∷ T rs) (a ∷ T ra) where η ∷ a ⊸ ST s a
class (Monad s a c, Monad s b c) ⇒ Lift2 (s ∷ T rs) (a ∷ T ra) (b ∷ T rb) (c ∷ T rc) where
  η2 ∷ (a ⊸ b ⊸ c) ⊸ ST s a ⊸ ST s b ⊸ ST s c

#define INST_LIFT2(S,A,B,C) \
instance Lift2 (s ∷ K S) (a ∷ K A) (b ∷ K B) (c ∷ K C) where {\
  η2 f sta stb = sta ⇉ \a → stb ⇉ \b → η (f a b)}

#define INST_PURE(S,A) \
instance Pure (s ∷ K (S)) (a ∷ K (A)) where {\
  η x = \s → (# s , x #)}

  --η2 f sta stb = sta ⇉ \a → stb ⇉ \b → η (f a b)}


class Run (a ∷ T ra) where run ∷ IO a ⊸ a
#define INST_RUN(A) \
instance Run (a ∷ K (A)) where {\
  run io = go (io ☸) where \
    go ∷ (# (☸) , a #) ⊸ a; \
    go (# s , a #) = (\(##) a → a) (escape# s) a}

class Pure s b ⇒ Monad (s ∷ T rs) (a ∷ T ra) (b ∷ T rb) where
  (⇉) ∷ ST s a ⊸ (a ⊸ ST s b) ⊸ ST s b
  η1 ∷ (a ⊸ b) ⊸ ST s a ⊸ ST s b
  (*>) ∷ ST_ s ⊸ ST s a ⊸ ST s a
  (>>) ∷ ST s (##) ⊸ ST s a ⊸ ST s a
#define INST_MONAD(S,A,B) \
instance Monad (s ∷ K (S)) (a ∷ K (A)) (b ∷ K (B)) where {\
  η1 f st = st ⇉ \a → return (f a); \
  (st *> sta) s = sta (st s); \
  (st >> sta) s = (\ (# s , (##) #) → sta s) (st s); \
  (st ⇉ f) s = go (st s) f where \
    go ∷ (# s , a #) ⊸ (a ⊸ ST s b) ⊸ (# s , b #); \
    go (# s' , a #) f = f a s'}


#define INSTS_PURE(S) \
INST_PURE(S,(##)); \
INST_PURE(S,()); \
INST_PURE(S,A#); \
INST_PURE(S,I); \
INST_PURE(S,U); \
INST_PURE(S,P#); \
INST_PURE(S,F32); \
INST_PURE(S,F64)

#define INSTS2_MONAD(S,Y) \
INST_MONAD(S,Y,(##)); \
INST_MONAD(S,Y,()); \
INST_MONAD(S,Y,A#); \
INST_MONAD(S,Y,I); \
INST_MONAD(S,Y,U); \
INST_MONAD(S,Y,P#); \
INST_MONAD(S,Y,F32); \
INST_MONAD(S,Y,F64)

#define INSTS_MONAD(S) \
INSTS2_MONAD(S,(##)); \
INSTS2_MONAD(S,()); \
INSTS2_MONAD(S,A#); \
INSTS2_MONAD(S,I); \
INSTS2_MONAD(S,U); \
INSTS2_MONAD(S,P#); \
INSTS2_MONAD(S,F32); \
INSTS2_MONAD(S,F64)

#define INSTS3_LIFT(S,Y,Z) \
INST_LIFT2(S,Y,Z,(##)); \
INST_LIFT2(S,Y,Z,()); \
INST_LIFT2(S,Y,Z,A#); \
INST_LIFT2(S,Y,Z,I); \
INST_LIFT2(S,Y,Z,U); \
INST_LIFT2(S,Y,Z,P#); \
INST_LIFT2(S,Y,Z,F32); \
INST_LIFT2(S,Y,Z,F64)

#define INSTS2_LIFT(S,Y) \
INSTS3_LIFT(S,Y,(##)); \
INSTS3_LIFT(S,Y,()); \
INSTS3_LIFT(S,Y,A#); \
INSTS3_LIFT(S,Y,I); \
INSTS3_LIFT(S,Y,U); \
INSTS3_LIFT(S,Y,P#); \
INSTS3_LIFT(S,Y,F32); \
INSTS3_LIFT(S,Y,F64)

#define INSTS_LIFT(S) \
INSTS2_LIFT(S,(##)); \
INSTS2_LIFT(S,()); \
INSTS2_LIFT(S,A#); \
INSTS2_LIFT(S,I); \
INSTS2_LIFT(S,U); \
INSTS2_LIFT(S,P#); \
INSTS2_LIFT(S,F32); \
INSTS2_LIFT(S,F64)

#define INSTS_DO(S) \
INSTS_PURE(S); \
INSTS_MONAD(S); \
INSTS_LIFT(S); \

INSTS_DO(())
INSTS_DO(P#)
INSTS_DO(A#)

INST_RUN((##))
INST_RUN(())
INST_RUN(A#)
INST_RUN(I)
INST_RUN(U)
INST_RUN(P#)
INST_RUN(F32)
INST_RUN(F64)
