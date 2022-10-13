{-# language UndecidableSuperClasses, InstanceSigs, CPP #-}
module Array.Index where
import Prelude hiding ((>=#))
import Prim
import Memset
import qualified GHC.Types as GHC
import Action
import Do.ST as ST
import Array
import Cast
import Cmp
import Num



-- | Operations for containers of contiguous primitive values.
type Index ∷ ∀ {r} {ra}. T r → (T r → T ra) → TC
class Elt a x ⇒ Index x a where
  -- | Index an element. 
  (!) ∷ a x → I {- ^ Offset in elements -} → x
  -- | Read an element.
  (!!) ∷ M a s x → I → ST s x
  -- | Write an element. 
  write ∷ M a s x → I → x → ST_ s
  -- | Set all elements.
  set# ∷ M a s x → I {- ^ offset -} → I {- ^ # elements to set -} → x → ST_ s

-- | Create new uninitialized arrays.
type New ∷ ∀ {rx}. (T rx → T_A) → TC
class Array a ⇒ New a where
  -- | Create a new array with all elements initialized to the same value.
  new ∷ Index x a ⇒ I {-^ size in elements -} → x → ST s (M a s x)
  -- | Create an array with all elements initialized by index
  gen ∷ Index x a ⇒ I → (I → x) → a x

#define INST_NEW_BOX(L)\
instance New (Array# ∷ T# L → T_A) where { ;\
  new = newArray# ;\
  gen n f = runST ST.do { ;\
   xs ← new# n ;\
   let go i = if i == n then \s→s else write xs i (f i) <> go (i + 1#) in go 0# ;\
   freeze## xs }} ;\
instance New (SmallArray# ∷ T# L → T_A) where { ;\
  new = newSmallArray#  ;\
  gen n f = runST ST.do { ;\
   xs ← new# n ;\
   let go i = if i == n then \s→s else write xs i (f i) <> go (i + 1#) in go 0# ;\
   freeze## xs }}

INST_NEW_BOX(Unlifted)
INST_NEW_BOX(Lifted)

#define INST_NEW_UB(X)\
instance New (A_ ∷ X → T_A) where { ;\
  new ∷ ∀ (x ∷ X) s. Index x A_ ⇒ I → x → ST s (M A_ s x) ;\
  new n e = ST.do { ;\
    ma ← new# n ;\
    set# ma 0# n e ;\
    return ma} ;\
  gen n f = runST ST.do { ;\
   xs ← new# n ;\
   let go i = if i == n then \s→s else write xs i (f i) <> go (i + 1#) in go 0# ;\
   freeze## xs}} \

INST_NEW_UB(T_I)
INST_NEW_UB(T_I1)
INST_NEW_UB(T_I2)
INST_NEW_UB(T_I4)
INST_NEW_UB(T_I8)
INST_NEW_UB(T_U)
INST_NEW_UB(T_U1)
INST_NEW_UB(T_U2)
INST_NEW_UB(T_U4)
INST_NEW_UB(T_U8)
INST_NEW_UB(T_F4)
INST_NEW_UB(T_F8)
INST_NEW_UB(T_P)

-- | "A.Box".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.indexLazy#'
--
-- @new@ uses sharing
instance Index x Array# where
  write = writeArray#
  (!!) = readArray#
  (!) a i = case indexArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of F# → \s → go (i +# 1#) (write a i x s)
                           T# → \s→s
instance Index (x ∷ T_A) Array# where
  write = writeArray#
  (!!) = readArray#
  (!) a i = case indexArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of F# → \s → go (i +# 1#) (write a i x s)
                           T# → \s→s

-- | "A.Boxed.Small".
--
-- @index#@ Forces the indexing but not the value. For more laziness use 'A.Box.Small.indexLazy#'
--
-- @new@ uses sharing
instance Index x SmallArray# where
  write = writeSmallArray#
  (!!) = readSmallArray# 
  (!) a i = case indexSmallArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of F# → \s → go (i +# 1#) (write a i x s)
                           T# → \s→s
instance Index (x ∷ T_A) SmallArray# where
  write = writeSmallArray# 
  (!!) = readSmallArray# 
  (!) a i = case indexSmallArray# a i of (# a #) → a
  set# a off n x = go 0# where
    go i = case i >=# n of F# → \s → go (i +# 1#) (write a i x s)
                           T# → \s→s

#define INST_IN(TY,IX,RD,WR,SET,IXP,RDP,WRP,SETP) \
instance (Coercible x TY) ⇒ Index x A_ where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))};\
instance (Coercible x TY) ⇒ Index x P_ where {\
  (!) = coerce IXP#; \
  (!!) = coerce RDP# ; \
  write = coerce WRP# ; \
  set# a i n x = unio (SETP# (coerce a) i n (coerce x))}
#define INST_IN_SPEC(TY,IX,RD,WR,SET,IXP,RDP,WRP,SETP) \
instance {-# OVERLAPPING #-} Index (TY) A_ where {\
  (!) = coerce IX#; \
  (!!) = coerce RD# ; \
  write = coerce WR# ; \
  set# a i n x = unio (SET# (coerce a) i n (coerce x))};\
instance {-# OVERLAPPING #-} Index (TY) P_ where {\
  (!) = coerce IXP#; \
  (!!) = coerce RDP# ; \
  write = coerce WRP# ; \
  set# a i n x = unio (SETP# (coerce a) i n (coerce x))}

INST_IN(I,indexIntArray,readIntArray,writeIntArray,setIntArray,indexIntOffAddr,readIntOffAddr,writeIntOffAddr,setIntOffAddr)
INST_IN(I1,indexInt8Array,readInt8Array,writeInt8Array,setInt8Array,indexInt8OffAddr,readInt8OffAddr,writeInt8OffAddr,setInt8OffAddr)
INST_IN(I2,indexInt16Array,readInt16Array,writeInt16Array,setInt16Array,indexInt16OffAddr,readInt16OffAddr,writeInt16OffAddr,setInt16OffAddr)
INST_IN(I4,indexInt32Array,readInt32Array,writeInt32Array,setInt32Array,indexInt32OffAddr,readInt32OffAddr,writeInt32OffAddr,setInt32OffAddr)
INST_IN(I8,indexInt64Array,readInt64Array,writeInt64Array,setInt64Array,indexInt64OffAddr,readInt64OffAddr,writeInt64OffAddr,setInt64OffAddr)
INST_IN(U,indexWordArray,readWordArray,writeWordArray,setWordArray,indexWordOffAddr,readWordOffAddr,writeWordOffAddr,setWordOffAddr)
INST_IN(U1,indexWord8Array,readWord8Array,writeWord8Array,setWord8Array,indexWord8OffAddr,readWord8OffAddr,writeWord8OffAddr,setWord8OffAddr)
INST_IN(U2,indexWord16Array,readWord16Array,writeWord16Array,setWord16Array,indexWord16OffAddr,readWord16OffAddr,writeWord16OffAddr,setWord16OffAddr)
INST_IN(U4,indexWord32Array,readWord32Array,writeWord32Array,setWord32Array,indexWord32OffAddr,readWord32OffAddr,writeWord32OffAddr,setWord32OffAddr)
INST_IN(U8,indexWord64Array,readWord64Array,writeWord64Array,setWord64Array,indexWord64OffAddr,readWord64OffAddr,writeWord64OffAddr,setWord64OffAddr)
INST_IN(F4,indexFloatArray,readFloatArray,writeFloatArray,setFloatArray,indexFloatOffAddr,readFloatOffAddr,writeFloatOffAddr,setFloatOffAddr)
INST_IN(F8,indexDoubleArray,readDoubleArray,writeDoubleArray,setDoubleArray,indexDoubleOffAddr,readDoubleOffAddr,writeDoubleOffAddr,setDoubleOffAddr)
INST_IN(Addr#,indexAddrArray,readAddrArray,writeAddrArray,setAddrArray,indexAddrOffAddr,readAddrOffAddr,writeAddrOffAddr,setAddrOffAddr)

INST_IN_SPEC(C1,indexCharArray,readCharArray,writeCharArray,setCharArray,indexCharOffAddr,readCharOffAddr,writeCharOffAddr,setCharOffAddr)
INST_IN_SPEC(C,indexWideCharArray,readWideCharArray,writeWideCharArray,setWideCharArray,indexWideCharOffAddr,readWideCharOffAddr,writeWideCharOffAddr,setWideCharOffAddr)

unio ∷ GHC.IO () → ST_ s
unio (GHC.IO io) s = case unsafeCoerce# io s of (# s' , _ #) → s'
{-# INLINE unio #-}

