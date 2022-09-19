{-# language CPP, NoImplicitPrelude #-}
module If (If(..)) where
import Types
import GHC.Prim
import GHC.Types

type If ∷ ∀ {r}. T r → Constraint
class If a where
  (?) ∷ B# → a → a → a
  ($) ∷ (a → a) → a → a

infixl 1 ?
infixr 0 $

#define INST_IF(A)\
instance If (a ∷ K (A)) where { \
  (?) (B# p) a b = if isTrue# p then a else b ;\
  ($) f b = f b }

#define INST_IF2(A,B)\
INST_IF((# A, B #)) ;\
INST_IF((# A | B #)) ;\
INST_IF3(A,B,I) ;\
INST_IF3(A,B,I8) ;\
INST_IF3(A,B,I16) ;\
INST_IF3(A,B,I32) ;\
INST_IF3(A,B,I64) ;\
INST_IF3(A,B,U) ;\
INST_IF3(A,B,U8) ;\
INST_IF3(A,B,U16) ;\
INST_IF3(A,B,U32) ;\
INST_IF3(A,B,U64) ;\
INST_IF3(A,B,F32) ;\
INST_IF3(A,B,F64) ;\
INST_IF3(A,B,P#) ;\
INST_IF3(A,B,Bytes) ;\
INST_IF3(A,B,()) ;\
INST_IF3(A,B,(##))

#define INST_IF3(A,B,C)\
INST_IF((# A,B,C #))



#define INST_IF1(A)\
INST_IF(A) ;\
INST_IF2(A,I) ;\
INST_IF2(A,I8) ;\
INST_IF2(A,I16) ;\
INST_IF2(A,I32) ;\
INST_IF2(A,I64) ;\
INST_IF2(A,U) ;\
INST_IF2(A,U8) ;\
INST_IF2(A,U16) ;\
INST_IF2(A,U32) ;\
INST_IF2(A,U64) ;\
INST_IF2(A,F32) ;\
INST_IF2(A,F64) ;\
INST_IF2(A,P#) ;\
INST_IF2(A,Bytes) ;\
INST_IF2(A,()) ;\
INST_IF2(A,(##))

INST_IF1(I)
INST_IF1(I8)
INST_IF1(I16)
INST_IF1(I32)
INST_IF1(I64)
INST_IF1(U)
INST_IF1(U8)
INST_IF1(U16)
INST_IF1(U32)
INST_IF1(U64)
INST_IF1(F32)
INST_IF1(F64)
INST_IF1(P#)
INST_IF1(Bytes)
INST_IF1(())
INST_IF1((##))
