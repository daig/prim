--------------------------------------------------------------------
-- | Description Utilities for working with linear functions on primitive types
--------------------------------------------------------------------
{-# language ScopedTypeVariables, UndecidableInstances #-}
{-# language NoImplicitPrelude #-}
module Linear where
import Types
import Unsafe.Coerce

class Linear (a ∷ T ra) (b ∷ T rb) where
  -- | Unsafely coerce a function on primitive values to be linear
  λ ∷ (a → b) → a ⊸ b
-- | Numeric values and pure data (eg functions) are identityless and can be copied freely.
class Linear a (# a, a #) ⇒ Dup (a ∷ T r) where δ ∷ a ⊸ (# a , a #)
#define INST_DUP(A) \
instance Dup (a ∷ K (A)) where δ = λ\x → (# x , x #)

#define INST2_LINEAR(A,B) \
instance Linear (a ∷ K (A)) (b ∷ K (B)) where {λ = unsafeCoerce#}

-- | Warning for boxed instance: It should only be used for functions and pure data.
INST_DUP(())
INST_DUP(I)
INST_DUP(U)
INST_DUP(F32)
INST_DUP(F64)


#define INSTS2_SUM_LINEAR(AA,BB) \
INST2_LINEAR(AA,(# BB | () #)); \
INST2_LINEAR(AA,(# BB | A# #)); \
INST2_LINEAR(AA,(# BB | I #)); \
INST2_LINEAR(AA,(# BB | I8 #)); \
INST2_LINEAR(AA,(# BB | I16 #)); \
INST2_LINEAR(AA,(# BB | I32 #)); \
INST2_LINEAR(AA,(# BB | U #)); \
INST2_LINEAR(AA,(# BB | U8 #)); \
INST2_LINEAR(AA,(# BB | U16 #)); \
INST2_LINEAR(AA,(# BB | U32 #)); \
INST2_LINEAR(AA,(# BB | P# #)); \
INST2_LINEAR(AA,(# BB | F32 #)); \
INST2_LINEAR(AA,(# BB | F64 #))

#define INSTS1_SUM_LINEAR(XX) \
INST2_LINEAR((# XX | XX #),(# XX | XX #)); \
INSTS2_SUM_LINEAR(XX,()); \
INSTS2_SUM_LINEAR(XX,A#); \
INSTS2_SUM_LINEAR(XX,I); \
INSTS2_SUM_LINEAR(XX,I8); \
INSTS2_SUM_LINEAR(XX,I16); \
INSTS2_SUM_LINEAR(XX,I32); \
INSTS2_SUM_LINEAR(XX,U); \
INSTS2_SUM_LINEAR(XX,U8); \
INSTS2_SUM_LINEAR(XX,U16); \
INSTS2_SUM_LINEAR(XX,U32); \
INSTS2_SUM_LINEAR(XX,P#); \
INSTS2_SUM_LINEAR(XX,F32); \
INSTS2_SUM_LINEAR(XX,(F64))

#define INSTS2_PROD_LINEAR(AA,BB) \
INST2_LINEAR(AA, (# BB #)); \
INST2_LINEAR(AA, (# (##) , (# I , BB #) #)); \
INST2_LINEAR(AA, (# (##) , (# I8 , BB #) #)); \
INST2_LINEAR(AA, (# (##) , (# I16 , BB #) #)); \
INST2_LINEAR(AA, (# (##) , (# I32 , BB #) #)); \
INST2_LINEAR((# AA , BB #),(# AA, BB #)); \
INST2_LINEAR(AA,(# BB , (##) #)); \
INST2_LINEAR(AA,(# BB , () #)); \
INST2_LINEAR(AA,(# BB , A# #)); \
INST2_LINEAR(AA,(# BB , I #)); \
INST2_LINEAR(AA,(# BB , I8 #)); \
INST2_LINEAR(AA,(# BB , I16 #)); \
INST2_LINEAR(AA,(# BB , I32 #)); \
INST2_LINEAR(AA,(# BB , U #)); \
INST2_LINEAR(AA,(# BB , U8 #)); \
INST2_LINEAR(AA,(# BB , U16 #)); \
INST2_LINEAR(AA,(# BB , U32 #)); \
INST2_LINEAR(AA,(# BB , P# #)); \
INST2_LINEAR(AA,(# BB , F32 #)); \
INST2_LINEAR(AA,(# BB , F64 #))


#define INSTS1_PROD_LINEAR(XX) \
INSTS2_PROD_LINEAR(XX,(##)); \
INSTS2_PROD_LINEAR((##),(# (##) , (# XX, XX #) #)); \
INSTS2_PROD_LINEAR(XX,()); \
INSTS2_PROD_LINEAR(XX,I); \
INSTS2_PROD_LINEAR(XX,I8); \
INSTS2_PROD_LINEAR(XX,I16); \
INSTS2_PROD_LINEAR(XX,I32); \
INSTS2_PROD_LINEAR(XX,A#); \
INSTS2_PROD_LINEAR(XX,P#); \
INSTS2_PROD_LINEAR(XX,U); \
INSTS2_PROD_LINEAR(XX,U8); \
INSTS2_PROD_LINEAR(XX,U16); \
INSTS2_PROD_LINEAR(XX,U32); \
INSTS2_PROD_LINEAR(XX,F32); \
INSTS2_PROD_LINEAR(XX,(F64))


#define INSTS1_LINEAR(AA) \
INSTS1_PROD_LINEAR(AA); \
INSTS1_SUM_LINEAR(AA); \
INST2_LINEAR(AA ,(# #)); \
INST2_LINEAR(AA,()); \
INST2_LINEAR(AA,A#); \
INST2_LINEAR(AA,I); \
INST2_LINEAR(AA,I8); \
INST2_LINEAR(AA,I16); \
INST2_LINEAR(AA,I32); \
INST2_LINEAR(AA,U); \
INST2_LINEAR(AA,U8); \
INST2_LINEAR(AA,U16); \
INST2_LINEAR(AA,U32); \
INST2_LINEAR(AA,P#); \
INST2_LINEAR(AA,F32); \
INST2_LINEAR(AA,(F64))

INSTS1_LINEAR((##))
INSTS1_LINEAR(())
INSTS1_LINEAR(A#)
INSTS1_LINEAR(I)
INSTS1_LINEAR(I8)
INSTS1_LINEAR(I16)
INSTS1_LINEAR(I32)
INSTS1_LINEAR(U)
INSTS1_LINEAR(U8)
INSTS1_LINEAR(U16)
INSTS1_LINEAR(U32)
INSTS1_LINEAR(P#)
INSTS1_LINEAR(F32)
INSTS1_LINEAR((F64))

INST2_LINEAR((# A# , I , I #),I)
INST2_LINEAR((# A# , I , I #),())
