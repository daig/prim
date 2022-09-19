module Do.Result where
import Do.ST qualified as ST

type Do ∷ ∀ {re} {ra} {rb}. T re → T ra → T rb → C
class Do e a b where
  (>>) ∷ Result e a → Result e b → Result e b
  (>>=) ∷ Result e a → (a → Result e b) → Result e b
  return ∷ a → Result e a

#define INST_RESULTDO(E,A,B)\
instance Do (e ∷ K (E)) (a ∷ K (A)) (b ∷ K (B)) where { ;\
  (# e | #) >> _ = (# e | #) ;\
  _ >> b = b ;\
  (# e | #) >>= _ = (# e | #) ;\
  (# | a #) >>= f = f a ;\
  return a = (# | a #) }

#define INSTS2_RESULTDO(E,Y) \
INST_RESULTDO(E,Y,(##)); \
INST_RESULTDO(E,Y,()); \
INST_RESULTDO(E,Y,Bytes); \
INST_RESULTDO(E,Y,I); \
INST_RESULTDO(E,Y,U); \
INST_RESULTDO(E,Y,Addr#); \
INST_RESULTDO(E,Y,F32); \
INST_RESULTDO(E,Y,F64)

#define INSTS3_RESULTDO(E) \
INSTS2_RESULTDO(E,(##)) ;\
INSTS2_RESULTDO(E,()) ;\
INSTS2_RESULTDO(E,Bytes) ;\
INSTS2_RESULTDO(E,I) ;\
INSTS2_RESULTDO(E,U) ;\
INSTS2_RESULTDO(E,Addr#) ;\
INSTS2_RESULTDO(E,F32) ;\
INSTS2_RESULTDO(E,F64)

INSTS3_RESULTDO((##))
INSTS3_RESULTDO(())
INSTS3_RESULTDO(Bytes)
INSTS3_RESULTDO(I)
INSTS3_RESULTDO(U)
INSTS3_RESULTDO(Addr#)
--INSTS3_RESULTDO(F32)
--INSTS3_RESULTDO(F64)
