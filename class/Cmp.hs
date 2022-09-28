{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE CPP #-}
module Cmp where
import Coerce
import {-# source #-} Bits
import Prelude hiding ((<#),(>#))
import GHC.Prim qualified as GHC
import Cast
import GHC.Prim qualified as GHC

infix 4 >, ≥, <, ≤, ≡, ≠, `cmp`
type (≡) ∷ ∀ {r}. T r → C
class (≡) (a ∷ T r) where
  (≡), (≠) ∷ a → a → B
  (=#), (≠#) ∷ a → a → B#
type (≤) ∷ ∀ {r}. T r → C
class (≡) a ⇒ (≤) a where
  (>),(≥),(<),(≤) ∷ a → a → B
  (>#),(≥#),(<#),(≤#) ∷ a → a → B#
  cmp ∷ a → a → Ordering
  -- | Minimum value
  min ∷ a → a → a
  -- | Maximum value
  max ∷ a → a → a

deriving newtype instance (≡) B#
deriving newtype instance (≤) B#
deriving newtype instance (≡) Ordering


instance (≡) I where
  (=#) = coerce (==#)
  (≠#) = coerce (/=#)
  (≡) = cast ((=#) @I)
  (≠) = cast ((≠#) @I)
instance (≤) I where
  (>#) = coerce (GHC.>#)
  (≥#) = coerce (>=#)
  (<#) = coerce (GHC.<#)
  (≤#) = coerce (<=#)
  (>) = cast ((>#) @I)
  (≥) = cast ((≥#) @I)
  (<) = cast ((<#) @I)
  (≤) = cast ((≤#) @I)
  cmp a b = Ordering# do (a GHC.># b) +# (a >=# b) -# 1#
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) I8 where
  (=#) = coerce eqInt8#
  (≠#) = coerce neInt8#
  (≡) = cast ((=#) @I8)
  (≠) = cast ((≠#) @I8)
instance (≤) I8 where
  (>#) = coerce gtInt8#
  (≥#) = coerce geInt8#
  (<#) = coerce ltInt8#
  (≤#) = coerce leInt8#
  (>) = cast ((>#) @I8)
  (≥) = cast ((≥#) @I8)
  (<) = cast ((<#) @I8)
  (≤) = cast ((≤#) @I8)
  cmp a b = Ordering# do gtInt8# a b GHC.-# ltInt8# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) I16 where
  (=#) = coerce eqInt16#
  (≠#) = coerce neInt16#
  (≡) = cast ((=#) @I16)
  (≠) = cast ((≠#) @I16)
instance (≤) I16 where
  (>#) = coerce gtInt16#
  (≥#) = coerce geInt16#
  (<#) = coerce ltInt16#
  (≤#) = coerce leInt16#
  (>) = cast ((>#) @I16)
  (≥) = cast ((≥#) @I16)
  (<) = cast ((<#) @I16)
  (≤) = cast ((≤#) @I16)
  cmp a b = Ordering# do gtInt16# a b GHC.-# ltInt16# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) I32 where
  (=#) = coerce eqInt32#
  (≠#) = coerce neInt32#
  (≡) = cast ((=#) @I32)
  (≠) = cast ((≠#) @I32)
instance (≤) I32 where
  (>#) = coerce gtInt32#
  (≥#) = coerce geInt32#
  (<#) = coerce ltInt32#
  (≤#) = coerce leInt32#
  (>) = cast ((>#) @I32)
  (≥) = cast ((≥#) @I32)
  (<) = cast ((<#) @I32)
  (≤) = cast ((≤#) @I32)
  cmp a b = Ordering# do gtInt32# a b GHC.-# ltInt32# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) I64 where
  (=#) = coerce eqInt64#
  (≠#) = coerce neInt64#
  (≡) = cast ((=#) @I64)
  (≠) = cast ((≠#) @I64)
instance (≤) I64 where
  (>#) = coerce gtInt64#
  (≥#) = coerce geInt64#
  (<#) = coerce ltInt64#
  (≤#) = coerce leInt64#
  (>) = cast ((>#) @I64)
  (≥) = cast ((≥#) @I64)
  (<) = cast ((<#) @I64)
  (≤) = cast ((≤#) @I64)
  cmp a b = Ordering# do gtInt64# a b GHC.-# ltInt64# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) U where
  (=#) = coerce eqWord#
  (≠#) = coerce neWord#
  (≡) = cast ((=#) @U)
  (≠) = cast ((≠#) @U)
instance (≤) U where
  (>#) = coerce gtWord#
  (≥#) = coerce geWord#
  (<#) = coerce ltWord#
  (≤#) = coerce leWord#
  (>) = cast ((>#) @U)
  (≥) = cast ((≥#) @U)
  (<) = cast ((<#) @U)
  (≤) = cast ((≤#) @U)
  cmp a b = Ordering# do gtWord# a b GHC.-# ltWord# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) U8 where
  (=#) = coerce eqWord8#
  (≠#) = coerce neWord8#
  (≡) = cast ((=#) @U8)
  (≠) = cast ((≠#) @U8)
instance (≤) U8 where
  (>#) = coerce gtWord8#
  (≥#) = coerce geWord8#
  (<#) = coerce ltWord8#
  (≤#) = coerce leWord8#
  (>) = cast ((>#) @U8)
  (≥) = cast ((≥#) @U8)
  (<) = cast ((<#) @U8)
  (≤) = cast ((≤#) @U8)
  cmp a b = Ordering# do gtWord8# a b GHC.-# ltWord8# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) U16 where
  (=#) = coerce eqWord16#
  (≠#) = coerce neWord16#
  (≡) = cast ((=#) @U16)
  (≠) = cast ((≠#) @U16)
instance (≤) U16 where
  (>#) = coerce gtWord16#
  (≥#) = coerce geWord16#
  (<#) = coerce ltWord16#
  (≤#) = coerce leWord16#
  (>) = cast ((>#) @U16)
  (≥) = cast ((≥#) @U16)
  (<) = cast ((<#) @U16)
  (≤) = cast ((≤#) @U16)
  cmp a b = Ordering# do gtWord16# a b GHC.-# ltWord16# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) U32 where
  (=#) = coerce eqWord32#
  (≠#) = coerce neWord32#
  (≡) = cast ((=#) @U32)
  (≠) = cast ((≠#) @U32)
instance (≤) U32 where
  (>#) = coerce gtWord32#
  (≥#) = coerce geWord32#
  (<#) = coerce ltWord32#
  (≤#) = coerce leWord32#
  (>) = cast ((>#) @U32)
  (≥) = cast ((≥#) @U32)
  (<) = cast ((<#) @U32)
  (≤) = cast ((≤#) @U32)
  cmp a b = Ordering# do gtWord32# a b GHC.-# ltWord32# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) U64 where
  (=#) = coerce eqWord64#
  (≠#) = coerce neWord64#
  (≡) = cast ((=#) @U64)
  (≠) = cast ((≠#) @U64)
instance (≤) U64 where
  (>#) = coerce gtWord64#
  (≥#) = coerce geWord64#
  (<#) = coerce ltWord64#
  (≤#) = coerce leWord64#
  (>) = cast ((>#) @U64)
  (≥) = cast ((≥#) @U64)
  (<) = cast ((<#) @U64)
  (≤) = cast ((≤#) @U64)
  cmp a b = Ordering# do gtWord64# a b GHC.-# ltWord64# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) Char# where
  (=#) = coerce eqChar#
  (≠#) = coerce neChar#
  (≡) = cast ((=#) @Char#)
  (≠) = cast ((≠#) @Char#)
instance (≤) Char# where
  (>#) = coerce gtChar#
  (≥#) = coerce geChar#
  (<#) = coerce ltChar#
  (≤#) = coerce leChar#
  (>) = cast ((>#) @Char#)
  (≥) = cast ((≥#) @Char#)
  (<) = cast ((<#) @Char#)
  (≤) = cast ((≤#) @Char#)
  cmp a b = Ordering# do gtChar# a b GHC.-# ltChar# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y
deriving newtype instance (≡) Char8#
deriving newtype instance (≤) Char8#

instance (≡) F32 where
  (=#) = coerce eqFloat#
  (≠#) = coerce neFloat#
  (≡) = cast ((=#) @F32)
  (≠) = cast ((≠#) @F32)
instance (≤) F32 where
  (>#) = coerce gtFloat#
  (≥#) = coerce geFloat#
  (<#) = coerce ltFloat#
  (≤#) = coerce leFloat#
  (>) = cast ((>#) @F32)
  (≥) = cast ((≥#) @F32)
  (<) = cast ((<#) @F32)
  (≤) = cast ((≤#) @F32)
  cmp a b = Ordering# do gtFloat# a b GHC.-# ltFloat# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y
instance (≡) F64 where
  (=#) = coerce (==##)
  (≠#) = coerce (/=##)
  (≡) = cast ((=#) @F64)
  (≠) = cast ((≠#) @F64)
instance (≤) F64 where
  (>#) = coerce (>##)
  (≥#) = coerce (>=##)
  (<#) = coerce (<##)
  (≤#) = coerce (<=##)
  (>) = cast ((>#) @F64)
  (≥) = cast ((≥#) @F64)
  (<) = cast ((<#) @F64)
  (≤) = cast ((≤#) @F64)
  cmp a b = Ordering# do (a >## b) +# (a >=## b) GHC.-# 1#
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

-- | _Value_ equality
instance (≡) ByteArray# where
  a =# b = let n = coerce sizeofByteArray# a in
           case n ==# coerce sizeofByteArray# b of
                  0# → F#
                  1# → coerce compareByteArrays# a 0# b 0# n =# 0#
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @ByteArray#)
  (≠) = cast ((≠#) @ByteArray#)
-- | _Value_ equality
deriving via ByteArray# instance (≡) (UnboxedArray# (x ∷ T r))
-- | _Reference_ equality
instance (≡) (PinnedMutableArray# s x) where
  (=#) = coerce sameMutableByteArray#
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(PinnedMutableArray# s x))
  (≠) = cast ((≠#) @(PinnedMutableArray# s x))
instance (≡) (PinnedArray# x) where
  (=#) = coerce sameByteArray#
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(PinnedArray# x))
  (≠) = cast ((≠#) @(PinnedArray# x))
  
-- | _Reference_ equality
instance (≡) (SmallArray# x) where
  (=#) = coerce (sameSmallArray# @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(SmallArray# x))
  (≠) = cast ((≠#) @(SmallArray# x))
-- | _Reference_ equality
instance (≡) (SmallMutableArray# s x) where
  (=#) = coerce (sameSmallMutableArray# @_ @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(SmallMutableArray# s x))
  (≠) = cast ((≠#) @(SmallMutableArray# s x))
deriving newtype instance (≡) (ConstRef x)
deriving newtype instance (≡) (SmallConstRef x)
deriving newtype instance (≡) (Ref s x)
deriving newtype instance (≡) (SmallRef s x)
deriving via (# PinnedArray# x, I #) instance (≡) (PinnedConstRef x)
deriving via (# PinnedMutableArray# s x, I #) instance (≡) (PinnedRef s x)

-- | _Reference_ equality
instance (≡) (Array# x) where
  (=#) = coerce (sameArray# @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(Array# x))
  (≠) = cast ((≠#) @(Array# x))
-- | _Reference_ equality
instance (≡) (MutableArray# s x) where
  (=#) = coerce (sameMutableArray# @_ @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(MutableArray# s x))
  (≠) = cast ((≠#) @(MutableArray# s x))
-- | _Reference_ equality
instance (≡) (MutVar# s x) where
  (=#) = coerce (sameMutVar# @s @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(MutVar# s x))
  (≠) = cast ((≠#) @(MutVar# s x))
-- | _Reference_ equality
instance (≡) (P_Async x) where
  (=#) = coerce (sameTVar# @_ @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(P_Async x))
  (≠) = cast ((≠#) @(P_Async x))
-- | _Reference_ equality
instance (≡) (P_Sync x) where
  (=#) = coerce (sameMVar# @_ @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(P_Sync x))
  (≠) = cast ((≠#) @(P_Sync x))
-- | _Reference_ equality
instance (≡) (P_Stable x) where
  (=#) = coerce (eqStablePtr# @x)
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(P_Stable x))
  (≠) = cast ((≠#) @(P_Stable x))
-- | _Reference_ equality
instance (≡) (P_Stable_Name a) where
  (=#) = coerce eqStableName#
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(P_Stable_Name a))
  (≠) = cast ((≠#) @(P_Stable_Name a))
-- | _Value_ equality
instance (≡) (UnboxedSlice x) where
  a =# b = case cmp a b of EQ → T#; _ → F#
  as ≠# bs = (¬) (as =# bs)
  (≡) = cast ((=#) @(UnboxedSlice x))
  (≠) = cast ((≠#) @(UnboxedSlice x))
-- | _Reference_ equality
deriving via (# PinnedArray# x, I, I #) instance (≡) (PinnedSlice x)

instance (≤) (PinnedSlice x) where
  cmp x y = if x ≡ y then EQ else coerce (cmp @(UnboxedSlice x)) x y
  a <# b = cmp a b =# LT
  a ># b = cmp a b =# GT
  a ≥# b = cmp a b ≠# LT
  a ≤# b = cmp a b ≠# GT
  a < b = cmp a b ≡ LT
  a > b = cmp a b ≡ GT
  a ≥ b = cmp a b ≠ LT
  a ≤ b = cmp a b ≠ GT
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y
  
instance (≤) (UnboxedSlice x) where
  Bytes_Off_Len# (# a, i, n #) `cmp` Bytes_Off_Len# (# b , j, m #)
    = case Ordering# (compareByteArrays# a i b j (n `min` m)) of {EQ → cmp n m; x → x}
  a <# b = cmp a b =# LT
  a ># b = cmp a b =# GT
  a ≥# b = cmp a b ≠# LT
  a ≤# b = cmp a b ≠# GT
  (>) = cast ((>#) @(UnboxedSlice x))
  (≥) = cast ((≥#) @(UnboxedSlice x))
  (<) = cast ((<#) @(UnboxedSlice x))
  (≤) = cast ((≤#) @(UnboxedSlice x))
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y

instance (≡) Addr# where
  (=#) = coerce eqAddr# ; (≠#) = coerce neAddr#
  (≡) = cast ((=#) @Addr#)
  (≠) = cast ((≠#) @Addr#)
instance (≤) Addr# where
  (>#) = coerce gtAddr#
  (≥#) = coerce geAddr#
  (<#) = coerce ltAddr#
  (≤#) = coerce leAddr#
  a < b = cmp a b ≡ LT
  a > b = cmp a b ≡ GT
  a ≥ b = cmp a b ≠ LT
  a ≤ b = cmp a b ≠ GT
  cmp a b = Ordering# do gtAddr# a b GHC.-# ltAddr# a b
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y
deriving via Addr# instance (≡) (ForeignArray# x)
deriving via Addr# instance (≤) (ForeignArray# x)
deriving via Addr# instance (≡) (ForeignMutableArray# s x)
deriving via Addr# instance (≤) (ForeignMutableArray# s x)
deriving newtype instance (≡) (ForeignSlice x)
deriving newtype instance (≡) (ForeignMutableSlice s x)
instance (≤) (ForeignSlice x) where
  Addr_Len# (# p, n #) `cmp` Addr_Len# (# q, m #) = case cmp p q of
    EQ → cmp n m
    x → x
  a < b = cmp a b ≡ LT
  a > b = cmp a b ≡ GT
  a ≥ b = cmp a b ≠ LT
  a ≤ b = cmp a b ≠ GT
  a <# b = cmp a b =# LT
  a ># b = cmp a b =# GT
  a ≥# b = cmp a b ≠# LT
  a ≤# b = cmp a b ≠# GT
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y
instance (≤) (ForeignMutableSlice s x) where
  MAddr_Len# (# p, n #) `cmp` MAddr_Len# (# q, m #) = case cmp p q of
    EQ → cmp n m
    x → x
  a < b = cmp a b ≡ LT
  a > b = cmp a b ≡ GT
  a ≥ b = cmp a b ≠ LT
  a ≤ b = cmp a b ≠ GT
  a <# b = cmp a b =# LT
  a ># b = cmp a b =# GT
  a ≥# b = cmp a b ≠# LT
  a ≤# b = cmp a b ≠# GT
  min x y = if x ≤ y then x else y
  max x y = if x ≥ y then x else y



#ifndef TUPLE_INSTS
instance ((≡) x, (≡) y, (≡) z) ⇒ (≡) (# (x ∷ K ByteArray#), (y ∷ K I) , (z ∷ K I) #) where
  (# x1, x2, x3 #) =# (# y1, y2, y3 #) = x1 =# y1 ∧ x2 =# y2 ∧ x3 =# y3
  as ≠# bs = (¬) (as =# bs)
  (# x1, x2, x3 #) ≡ (# y1, y2, y3 #) = cast (x1 =# y1 ∧ x2 =# y2 ∧ x3 =# y3)
  as ≠ bs = cast ((¬) (as =# bs))

instance ((≡) x,(≡) y) ⇒ (≡) (# (x ∷ K Addr#), (y ∷ K I) #) where
  (# x1, x2 #) =# (# y1, y2 #) = x1 =# y1 ∧ x2 =# y2
  as ≠# bs = (¬) (as =# bs)
  (# x1, x2 #) ≡ (# y1, y2 #) = cast (x1 =# y1 ∧ x2 =# y2)
  as ≠ bs = cast ((¬) (as =# bs))

instance ((≡) x,(≡) y) ⇒ (≡) (# (x ∷ K ByteArray#), (y ∷ K I) #) where
  (# x1, x2 #) =# (# y1, y2 #) = x1 =# y1 ∧ x2 =# y2
  as ≠# bs = (¬) (as =# bs)
  (# x1, x2 #) ≡ (# y1, y2 #) = cast (x1 =# y1 ∧ x2 =# y2)
  as ≠ bs = cast ((¬) (as =# bs))
#endif

-- Define equality for all tuples up to size 3
#ifdef TUPLE_INSTS

#define INST_EQ3(X,Y,Z)\
instance ((≡) x, (≡) y, (≡) z) ⇒ (≡) (# (x ∷ K X), (y ∷ K Y) , (z ∷ K Z) #) where { ;\
  (# x1, x2, x3 #) ≡ (# y1, y2, y3 #) = x1 ≡ y1 ∧ x2 ≡ y2 ∧ x3 ≡ y3 ;\
  as ≠# bs = (¬) (as =# bs) }

#define INST_EQ2(X,Y)\
instance ((≡) x,(≡) y) ⇒ (≡) (# (x ∷ K X), (y ∷ K Y) #) where { ;\
  (# x1, x2 #) ≡ (# y1, y2 #) = x1 ≡ y1 ∧ x2 ≡ y2 ;\
  as ≠# bs = (¬) (as =# bs) } ;\
INST_EQ3(X,Y,I);\
INST_EQ3(X,Y,I8);\
INST_EQ3(X,Y,I16);\
INST_EQ3(X,Y,I32);\
INST_EQ3(X,Y,I64);\
INST_EQ3(X,Y,U);\
INST_EQ3(X,Y,U8);\
INST_EQ3(X,Y,U16);\
INST_EQ3(X,Y,U32);\
INST_EQ3(X,Y,U64);\
INST_EQ3(X,Y,F32);\
INST_EQ3(X,Y,F64);\
INST_EQ3(X,Y,(##));\
INST_EQ3(X,Y,Addr#);\
INST_EQ3(X,Y,());\
INST_EQ3(X,Y,ByteArray#)

#define INST_EQ(X)\
INST_EQ2(X,I);\
INST_EQ2(X,I8);\
INST_EQ2(X,I16);\
INST_EQ2(X,I32);\
INST_EQ2(X,I64);\
INST_EQ2(X,U);\
INST_EQ2(X,U8);\
INST_EQ2(X,U16);\
INST_EQ2(X,U32);\
INST_EQ2(X,U64);\
INST_EQ2(X,F32);\
INST_EQ2(X,F64);\
INST_EQ2(X,(##));\
INST_EQ2(X,Addr#);\
INST_EQ2(X,());\
INST_EQ2(X,ByteArray#)


instance (≡) (x ∷ K (##)) where {_ ≡ _ = T#; _ ≠ _ = F#}

INST_EQ(I)
INST_EQ(I8)
INST_EQ(I16)
INST_EQ(I32)
INST_EQ(I64)
INST_EQ(U)
INST_EQ(U8)
INST_EQ(U16)
INST_EQ(U32)
INST_EQ(U64)
INST_EQ(F32)
INST_EQ(F64)
INST_EQ((##))
INST_EQ(Addr#)
INST_EQ(())
INST_EQ(ByteArray#)

#endif
