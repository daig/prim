{-# language InstanceSigs, CPP #-}
module Array.Map where
import Array
import Array.Index
import Num
import Do.ST as ST
import Cmp
import Var
import Action
import Cast
import GHC.CString

type Map ∷ ∀ {ra} {rb} {rv}. (∀ {r}. T r → T rv) → T ra → T rb → TC
class Map v a b where
  imap ∷ v a → (I → a → b) → v b
  map  ∷ v a → (a → b) → v b
  mapIO ∷ v a → (a → ST s b) → ST s (v b)
  imapIO ∷ v a → (I → a → ST s b) → ST s (v b)
type Fold ∷ ∀ {ra} {rb} {rv}. (T ra → T rv) → T ra → T rb → TC
class Fold v a b where
  fold ∷ v a → b → (b → a → b) → b
  ifold ∷ v a → b → (I → b → a → b) → b
type FoldIO ∷ ∀ {ra} {rb} {rv}. ★ → (T ra → T rv) → T ra → T rb → TC
class FoldIO s v a b where
  foldIO ∷ v a → b → (b → a → ST s b) → ST s b
  ifoldIO ∷ v a → b → (I → b → a → ST s b) → ST s b

-- | Lazy fold
type Foldr ∷ ∀ {r} {rv}. (T r → T rv) → T r → TC
class Foldr v x where foldr ∷ v x → b → (x → b → b) → b

instance Foldr S# C1# where
  foldr (coerce → p) b cbb = unpackFoldrCString# p (\c → cbb (cast @C1# @Char8 (coerce c))) b
instance Foldr S# C# where
  foldr (coerce → p) b cbb = unpackFoldrCStringUtf8# p (\c → cbb (cast c)) b


#define INST_EACH(A)\
instance Each s (UnboxedMutableArray# s ∷ K A → T_) A where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = lenM v >>* \n → gogo n 0# where { ;\
    gogo n = go where { ;\
        go i | i == n = \s→s ;\
        go i = v !! i >>* \x → f i x <> go (i-1#) }}} ;\
instance Each s (UnboxedArray# ∷ K A → T_) A where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = go 0# where { ;\
    go i | i == n = \s→s ;\
    go i = f i (v!i) <> go (i-1#) ;\
    n = len v}} ;\
instance Each s ForeignSlice A where { ;\
  ieach (Addr_Len# (# p, n #)) f = go 0# where { ;\
    go i | i == n = \s→s ;\
    go i = f i (ConstAddr# p ! i) <> go (i-1#)} ;\
  each (Addr_Len# (# p, n #)) f = go p where { ;\
    go p | p == end = \s→s ;\
    go p = f (ConstAddr# p ! 0#) <> go (p +. 1#);\
    end = p +. n}} ;\
instance Modify UnboxedMutableArray# A where { ;\
  xs %= f = ieach xs \ i x → write xs i (f x)} ;\

instance Modify MutVar# a where
  v %= f = \s → case atomicModifyMutVar_# v f s of (# s', _, _ #) → s'

instance Modify TVar# a where v %= f = read v >>* \x → v .= f x
instance Modify MVar# a where v %= f = read v >>* \x → v .= f x
instance Modify IOPort# a where v %= f = read v >>* \x → v .= f x

#define INST_MAP2_UB(A,B)\
instance Map UnboxedArray# A B where { ;\
  imap xs f = runST ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs \ i x → write ys i (f i x) ;\
    freeze## ys } ;\
  map xs f = runST ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs \ i x → write ys i (f x) ;\
    freeze## ys } ;\
  mapIO xs f = ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) ;\
    freeze## ys } ;\
  imapIO xs f = ST.do { ;\
    ys ← new# (len xs) ;\
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) ;\
    freeze## ys }};\
instance Fold UnboxedArray# A B where {;\
  fold v b0 bab = go 0# b0 where {;\
    n = len v ;\
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b };\
  ifold v b0 ibab = go 0# b0 where {;\
    n = len v ;\
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b }};\
instance FoldIO s UnboxedArray# A B where {;\
  foldIO v b0 bamb = go 0# b0 where {;\
    n = len v;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← bamb b (v!i);\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO v b0 ibamb = go 0# b0 where {;\
    n = len v;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← ibamb i b (v!i);\
        go (i + 1#) b'};\
      else return b }}};\
instance FoldIO s (UnboxedMutableArray# s) A B where {;\
  foldIO v b0 bamb = go 0# b0 where {;\
    go i b = ST.do {;\
      n ← lenM v;\
      if i < n then ST.do {;\
        x ← v !! i;\
        b' ← bamb b x;\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO v b0 ibamb = go 0# b0 where {;\
    go i b = ST.do {;\
      n ← lenM v;\
      if i < n then ST.do {;\
        x ← v !! i;\
        b' ← ibamb i b x;\
        go (i + 1#) b'};\
       else return b }}};\
instance FoldIO s ForeignSlice A B where { ;\
  foldIO (Addr_Len# (# ConstAddr# → v, n #)) b0 bamb = go 0# b0 where {;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← bamb b (v!i);\
        go (i + 1#) b'};\
       else return b }};\
  ifoldIO (Addr_Len# (# ConstAddr# → v, n #)) b0 ibamb = go 0# b0 where {;\
    go i b = ST.do {;\
      if i < n then ST.do {;\
        b' ← ibamb i b (v!i);\
        go (i + 1#) b'};\
      else return b }}}


#define INST_FOLD_S2(A,B);\
instance Fold S# A (x ∷ K B) where {;\
    fold (S# s) r0 f = go (ConstAddr# s) r0 where {go p r = let ch = p!0# in if ch == coerce '\0'# then r else go (p +. 1#) (f r ch)};\
    ifold (S# (ConstAddr# → p)) r0 f = go 0# r0 where {go i r = let ch = p!i in if ch == coerce '\0'# then r else go (i + 1#) (f i r ch)}}


#define INST_FOLD_S(A)\
INST_FOLD_S2(C1#,A);\
INST_FOLD_S2(C#,A)

#define INST_MAP_UB(A);\
INST_EACH(A);\
INST_FOLD_S(A);\
INST_MAP2_UB(A,I) ;\
INST_MAP2_UB(A,I1) ;\
INST_MAP2_UB(A,I2) ;\
INST_MAP2_UB(A,I4) ;\
INST_MAP2_UB(A,I8) ;\
INST_MAP2_UB(A,U) ;\
INST_MAP2_UB(A,U1) ;\
INST_MAP2_UB(A,U2) ;\
INST_MAP2_UB(A,U4) ;\
INST_MAP2_UB(A,U8) ;\
INST_MAP2_UB(A,F4) ;\
INST_MAP2_UB(A,F8) ;\
INST_MAP2_UB(A,Addr#) ;\
instance Foldr UnboxedArray# A where {;\
  foldr v b0 abb = go 0# where {;\
    n = len v;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}};\
instance Foldr ForeignSlice A where {;\
  foldr (Addr_Len# (# ConstAddr# → v, n #)) b0 abb = go 0# where {;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}}
instance Foldr Array# a where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr Array# (a ∷ T_) where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr SmallArray# a where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr SmallArray# (a ∷ T_) where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

INST_MAP_UB(I)
INST_MAP_UB(I1)
INST_MAP_UB(I2)
INST_MAP_UB(I4)
INST_MAP_UB(I8)
INST_MAP_UB(U)
INST_MAP_UB(U1)
INST_MAP_UB(U2)
INST_MAP_UB(U4)
INST_MAP_UB(U8)
INST_MAP_UB(F4)
INST_MAP_UB(F8)
INST_MAP_UB(Addr#)

type Each ∷ ∀ {r} {rv}. ★ → (T r → T rv) → T r → TC
class Each s v a where
  ieach ∷ v a → (I → a → ST_ s) → ST_ s
  each ∷ v a → (a → ST_ s) → ST_ s


type Modify ∷ ∀ {r} {rv}. (★ → T r → T rv) → T r → TC
class Modify v a where
  (%=) ∷ ∀ s. v s a → (a → a) → ST_ s


-- TODO <%= atomic modify 2 ioref
