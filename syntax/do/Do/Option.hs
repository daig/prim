module Do.Option where

type Do ∷ ∀ {ra} {rb}. T ra → T rb → C
class Do a b where
  (>>) ∷ (?) a → (?) b → (?) b
  (>>=) ∷ (?) a → (a → (?) b) → (?) b
  return ∷ a → (?) a

#define INST_OPTIONDO(A,B)\
instance Do (a ∷ K (A)) (b ∷ K (B)) where { ;\
  (# (##) | #) >> _ = (# (##) | #) ;\
  _ >> b = b ;\
  (# (##) | #) >>= _ = (# (##) | #) ;\
  (# | a #) >>= f = f a ;\
  return a = (# | a #) }

#define INSTS2_OPTIONDO(Y) \
INST_OPTIONDO(Y,(##)); \
INST_OPTIONDO(Y,()); \
INST_OPTIONDO(Y,Bytes); \
INST_OPTIONDO(Y,I); \
INST_OPTIONDO(Y,U); \
INST_OPTIONDO(Y,Addr#); \
INST_OPTIONDO(Y,F32); \
INST_OPTIONDO(Y,F64)

INSTS2_OPTIONDO((##))
INSTS2_OPTIONDO(())
INSTS2_OPTIONDO(Bytes)
INSTS2_OPTIONDO(I)
INSTS2_OPTIONDO(U)
INSTS2_OPTIONDO(Addr#)
INSTS2_OPTIONDO(F32)
INSTS2_OPTIONDO(F64)
