{-# language CPP #-}
module Array.Map where
import Array
import Array.Index
import Num
import Do.ST as ST
import Cmp
import Var
import Action

type Map ∷ ∀ {ra} {rb} {rv}. (∀ {r}. T r → T rv) → T ra → T rb → C
class Map v a b where
  imap ∷ (I → a → b) → v a → v b
  map  ∷ (a → b) → v a → v b
  mapIO ∷ (a → ST s b) → v a → ST s (v b)
  imapIO ∷ (I → a → ST s b) → v a → ST s (v b)
  fold ∷ (b → a → b) → b → v a → b
  ifold ∷ (I → b → a → b) → b → v a → b

-- | Lazy fold
type Foldr ∷ ∀ {r} {rv}. (T r → T rv) → T r → C
class Foldr v x where foldr ∷ (x → b → b) → b → v x → b



#define INST_EACH(A)\
instance Each s (UnboxedMutableArray# s ∷ K A → T_) A where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = ST.do n <- lenM v; gogo n 0# where { ;\
    gogo n = go where { ;\
        go i | i == n = ST.nop ;\
        go i = ST.do x ← v !! i; f i x *> go (i-1#) }} ;\
  foldIO bamb b0 v = go 0# b0 where {;\
    go i b = ST.do {;\
      n ← lenM v;\
      if i < n then ST.do {;\
        x ← v !! i;\
        b' ← bamb b x;\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO ibamb b0 v = go 0# b0 where {;\
    go i b = ST.do {;\
      n ← lenM v;\
      if i < n then ST.do {;\
        x ← v !! i;\
        b' ← ibamb i b x;\
        go (i + 1#) b'};\
       else return b }}};\
instance Each s (UnboxedArray# ∷ K A → T_) A where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = go 0# where { ;\
    go i | i == n = ST.nop ;\
    go i = f i (v ! i) *> go (i-1#) ;\
    n = len v} ;\
  foldIO bamb b0 v = go 0# b0 where {;\
    n = len v;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← bamb b (v!i);\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO ibamb b0 v = go 0# b0 where {;\
    n = len v;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← ibamb i b (v!i);\
        go (i + 1#) b'};\
      else return b }}};\
instance Each s (ForeignSlice ∷ K A → K (# Addr#, I #)) A where { ;\
  ieach (Addr_Len# (# p, n #)) f = go 0# where { ;\
    go i | i == n = ST.nop ;\
    go i = f i (ConstAddr# p ! i) *> go (i-1#)} ;\
  each (Addr_Len# (# p, n #)) f = go p where { ;\
    go p | p == end = ST.nop ;\
    go p = f (ConstAddr# p ! 0#) *> go (p +. 1#);\
    end = p +. n} ;\
  foldIO bamb b0 (Addr_Len# (# ConstAddr# → v, n #)) = go 0# b0 where {;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← bamb b (v!i);\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO ibamb b0 (Addr_Len# (# ConstAddr# → v, n #)) = go 0# b0 where {;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← ibamb i b (v!i);\
        go (i + 1#) b'};\
      else return b }}};\
instance Modify UnboxedMutableArray# A where { ;\
  xs %= f = ieach xs \ i x → write xs i (f x)} ;\

instance Modify MutVar# a where
  v %= f = \s → case atomicModifyMutVar_# v f s of (# s', _, _ #) → (# s', (##) #)

instance Modify TVar# a where v %= f = ST.do x ← read v; st (v .= f x)
instance Modify MVar# a where v %= f = ST.do x ← read v; st (v .= f x)
instance Modify IOPort# a where v %= f = ST.do x ← read v; st (v .= f x)

#define INST_MAP2_UB(A,B)\
instance Map UnboxedArray# A B where { ;\
  fold bab b0 v = go 0# b0 where {;\
    n = len v ;\
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b };\
  ifold ibab b0 v = go 0# b0 where {;\
    n = len v ;\
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b };\
  imap f xs = runST ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs \ i x → write ys i (f i x) ;\
    freeze## ys } ;\
  map f xs = runST ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs \ i x → write ys i (f x) ;\
    freeze## ys } ;\
  mapIO f xs = ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) ;\
    freeze## ys } ;\
  imapIO f xs = ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) ;\
    freeze## ys }}

#define INST_MAP_UB(A);\
INST_EACH(A);\
INST_MAP2_UB(A,I) ;\
INST_MAP2_UB(A,I8) ;\
INST_MAP2_UB(A,I16) ;\
INST_MAP2_UB(A,I32) ;\
INST_MAP2_UB(A,I64) ;\
INST_MAP2_UB(A,U) ;\
INST_MAP2_UB(A,U8) ;\
INST_MAP2_UB(A,U16) ;\
INST_MAP2_UB(A,U32) ;\
INST_MAP2_UB(A,U64) ;\
INST_MAP2_UB(A,F32) ;\
INST_MAP2_UB(A,F64) ;\
INST_MAP2_UB(A,Addr#) ;\
instance Foldr UnboxedArray# A where {;\
  foldr abb b0 v = go 0# where {;\
    n = len v;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}};\
instance Foldr ForeignSlice A where {;\
  foldr abb b0 (Addr_Len# (# ConstAddr# → v, n #)) = go 0# where {;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}}
instance Foldr Array# a where
  foldr abb b0 v = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr Array# (a ∷ T_) where
  foldr abb b0 v = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr SmallArray# a where
  foldr abb b0 v = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr SmallArray# (a ∷ T_) where
  foldr abb b0 v = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0



INST_MAP_UB(I)
INST_MAP_UB(I8)
INST_MAP_UB(I16)
INST_MAP_UB(I32)
INST_MAP_UB(I64)
INST_MAP_UB(U)
INST_MAP_UB(U8)
INST_MAP_UB(U16)
INST_MAP_UB(U32)
INST_MAP_UB(U64)
INST_MAP_UB(F32)
INST_MAP_UB(F64)
INST_MAP_UB(Addr#)

type Each ∷ ∀ {r} {rv}. ★ → (T r → T rv) → T r → C
class Each s v a where
  ieach ∷ v a → (I → a → ST_ s) → ST s (##)
  each ∷ v a → (a → ST_ s) → ST s (##)
  foldIO ∷ (b → a → ST s b) → b → v a → ST s b
  ifoldIO ∷ (I → b → a → ST s b) → b → v a → ST s b


type Modify ∷ ∀ {r} {rv}. (★ → T r → T rv) → T r → C
class Modify v a where
  (%=) ∷ ∀ s. v s a → (a → a) → ST s (##)


-- TODO <%= atomic modify 2 ioref

