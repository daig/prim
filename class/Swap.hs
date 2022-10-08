{-# language CPP,UndecidableSuperClasses #-}
module Swap where
import Bits

-- TODO change name of atomic swap

type Swap ∷ ∀ {ra} {rb}. T ra → T rb → C
class Swap b a ⇒ Swap a b where swap ∷ a → b

#define INST_SWAP2(X,Y)\
instance Swap (# (x ∷ K X), (y ∷ K Y) #) (# y, x #) where {swap (# i, j #) = (# j, i #)};\
instance Swap (# (y ∷ K Y), (x ∷ K X) #) (# x, y #) where {swap (# i, j #) = (# j, i #)}

#define INST_SWAP(X)\
instance Swap (# (##) | (x ∷ K X) #) (# x | (##) #) where { \
  swap x = case unsafeCoerce# x of (# tag, (y ∷ x) #) → unsafeCoerce# (# xor 0b11# tag, y #)} ;\
instance Swap (# (x ∷ K X) | (##) #) (# (##) | x #) where {\
  swap x = case unsafeCoerce# x of (# tag, (y ∷ x) #) → unsafeCoerce# (# xor 0b11# tag, y #) } ;\
instance Swap (# (x ∷ K X) | x #) (# x | x #) where {\
  swap x = case unsafeCoerce# x of (# tag, (y ∷ x) #) → unsafeCoerce# (# xor 0b11# tag, y #) } ;\

INST_SWAP(I)
INST_SWAP2(I,I1)
INST_SWAP2(I,I2)
INST_SWAP2(I,I4)
INST_SWAP2(I,I8)
INST_SWAP2(I,U)
INST_SWAP2(I,U1)
INST_SWAP2(I,U2)
INST_SWAP2(I,U4)
INST_SWAP2(I,U8)
INST_SWAP2(I,F4)
INST_SWAP2(I,F8)
INST_SWAP2(I,(##))
INST_SWAP2(I,())
INST_SWAP2(I,Addr#)
INST_SWAP2(I,ByteArray#)

INST_SWAP(I1)
INST_SWAP2(I1,I2)
INST_SWAP2(I1,I4)
INST_SWAP2(I1,I8)
INST_SWAP2(I1,U)
INST_SWAP2(I1,U1)
INST_SWAP2(I1,U2)
INST_SWAP2(I1,U4)
INST_SWAP2(I1,U8)
INST_SWAP2(I1,F4)
INST_SWAP2(I1,F8)
INST_SWAP2(I1,(##))
INST_SWAP2(I1,())
INST_SWAP2(I1,Addr#)
INST_SWAP2(I1,ByteArray#)

INST_SWAP(I2)
INST_SWAP2(I2,I4)
INST_SWAP2(I2,I8)
INST_SWAP2(I2,U)
INST_SWAP2(I2,U1)
INST_SWAP2(I2,U2)
INST_SWAP2(I2,U4)
INST_SWAP2(I2,U8)
INST_SWAP2(I2,F4)
INST_SWAP2(I2,F8)
INST_SWAP2(I2,(##))
INST_SWAP2(I2,())
INST_SWAP2(I2,Addr#)
INST_SWAP2(I2,ByteArray#)

INST_SWAP(I4)
INST_SWAP2(I4,I8)
INST_SWAP2(I4,U)
INST_SWAP2(I4,U1)
INST_SWAP2(I4,U2)
INST_SWAP2(I4,U4)
INST_SWAP2(I4,U8)
INST_SWAP2(I4,F4)
INST_SWAP2(I4,F8)
INST_SWAP2(I4,(##))
INST_SWAP2(I4,())
INST_SWAP2(I4,Addr#)
INST_SWAP2(I4,ByteArray#)

INST_SWAP(I8)
INST_SWAP2(I8,U)
INST_SWAP2(I8,U1)
INST_SWAP2(I8,U2)
INST_SWAP2(I8,U4)
INST_SWAP2(I8,U8)
INST_SWAP2(I8,F4)
INST_SWAP2(I8,F8)
INST_SWAP2(I8,(##))
INST_SWAP2(I8,())
INST_SWAP2(I8,Addr#)
INST_SWAP2(I8,ByteArray#)

INST_SWAP(U)
INST_SWAP2(U,U1)
INST_SWAP2(U,U2)
INST_SWAP2(U,U4)
INST_SWAP2(U,U8)
INST_SWAP2(U,F4)
INST_SWAP2(U,F8)
INST_SWAP2(U,(##))
INST_SWAP2(U,())
INST_SWAP2(U,Addr#)
INST_SWAP2(U,ByteArray#)

INST_SWAP(U1)
INST_SWAP2(U1,U2)
INST_SWAP2(U1,U4)
INST_SWAP2(U1,U8)
INST_SWAP2(U1,F4)
INST_SWAP2(U1,F8)
INST_SWAP2(U1,(##))
INST_SWAP2(U1,())
INST_SWAP2(U1,Addr#)
INST_SWAP2(U1,ByteArray#)

INST_SWAP(U2)
INST_SWAP2(U2,U4)
INST_SWAP2(U2,U8)
INST_SWAP2(U2,F4)
INST_SWAP2(U2,F8)
INST_SWAP2(U2,(##))
INST_SWAP2(U2,())
INST_SWAP2(U2,Addr#)
INST_SWAP2(U2,ByteArray#)

INST_SWAP(U4)
INST_SWAP2(U4,U8)
INST_SWAP2(U4,F4)
INST_SWAP2(U4,F8)
INST_SWAP2(U4,(##))
INST_SWAP2(U4,())
INST_SWAP2(U4,Addr#)
INST_SWAP2(U4,ByteArray#)

INST_SWAP(U8)
INST_SWAP2(U8,F4)
INST_SWAP2(U8,F8)
INST_SWAP2(U8,(##))
INST_SWAP2(U8,())
INST_SWAP2(U8,Addr#)
INST_SWAP2(U8,ByteArray#)

INST_SWAP(F4)
INST_SWAP2(F4,F8)
INST_SWAP2(F4,(##))
INST_SWAP2(F4,())
INST_SWAP2(F4,Addr#)
INST_SWAP2(F4,ByteArray#)

INST_SWAP(F8)
INST_SWAP2(F8,(##))
INST_SWAP2(F8,())
INST_SWAP2(F8,Addr#)
INST_SWAP2(F8,ByteArray#)

INST_SWAP((##))
INST_SWAP2((##),())
INST_SWAP2((##),Addr#)
INST_SWAP2((##),ByteArray#)

INST_SWAP(())
INST_SWAP2((),Addr#)
INST_SWAP2((),ByteArray#)

INST_SWAP(Addr#)
INST_SWAP2(Addr#,ByteArray#)

INST_SWAP(ByteArray#)

