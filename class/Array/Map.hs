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
import Bits

type Map ∷ ∀ {ra} {rb} {rv}. (∀ {r}. T r → T rv) → T ra → T rb → TC
class Map v a b where
  imap ∷ v a → (I → a → b) → v b
  map  ∷ v a → (a → b) → v b
type MapIO ∷ ∀ {ra} {rb} {rv}. ★ → (∀ {r}. T r → T rv) → T ra → T rb → TC
class MapIO s v a b where
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
type Each ∷ ∀ {r} {rv}. ★ → (T r → T rv) → T r → TC
class Each s v a where
  each ∷ v a → (a → ST_ s) → ST_ s
  ieach ∷ v a → (I → a → ST_ s) → ST_ s

-- | Lazy fold
type Foldr ∷ ∀ {r} {rv}. (T r → T rv) → T r → TC
class Foldr v x where foldr ∷ v x → b → (x → b → b) → b

instance Foldr S C1 where
  foldr (coerce → p) b cbb = unpackFoldrCString# p (\c → cbb (cast @C1 @Char8 (coerce c))) b
instance Foldr S C where
  foldr (coerce → p) b cbb = unpackFoldrCStringUtf8# p (\c → cbb (cast c)) b

instance Modify MutVar# a where
  v %= f = \s → case atomicModifyMutVar_# v f s of (# s', _, _ #) → s'

instance Modify TVar# a where v %= f = read v >>* \x → v .= f x
instance Modify MVar# a where v %= f = read v >>* \x → v .= f x
instance Modify IOPort# a where v %= f = read v >>* \x → v .= f x


instance Each s S C1 where
    each (S# s) st = go (P'# s) where
      go p = let ch = p!0#
               in if ch == coerce '\0'# then \s→s
               else st ch <> go (p +. 1#)
    ieach (S# (P'# → p)) st = go 0# where
      go i = let ch = p!i
               in if ch == coerce '\0'# then \s→s
               else st i ch <> go (i + 1#)
instance Each s S C where 
  each (S# p0) io = go (P'# @C1 p0)
    where
      go p = let C1# ch = p!0#
             in if ch == '\0'# then \s→s
             else let n = byteCount ch
                  in io (unpackUtf8C# n ch p) <> go (p +. n)
  ieach (S# (P'# → p)) io = go 0#
    where
      go i = let C1# ch = p!i
               in if ch == '\0'# then \s→s
               else let n = byteCount ch
                    in io i (unpackUtf8C# n ch p) <> go (i +. n)



#define INST_MAP_UB(X);\
instance Each s (A s ∷ K X → T_) X where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = lenM v >>* \n → gogo n 0# where { ;\
    gogo n = go where { ;\
        go i | i == n = \s→s ;\
        go i = v !! i >>* \x → f i x <> go (i-1#) }}} ;\
instance Each s A' X where { ;\
  each v f = ieach v (\_ → f) ;\
  ieach v f = go 0# where { ;\
    go i | i == n = \s→s ;\
    go i = f i (v!i) <> go (i-1#) ;\
    n = len v}} ;\
instance Each s P'## X where { ;\
  ieach (P'_Len# (# p, n #)) f = go 0# where { ;\
    go i | i == n = \s→s ;\
    go i = f i (P'# p ! i) <> go (i-1#)} ;\
  each (P'_Len# (# p, n #)) f = go p where { ;\
    go p | p == end = \s→s ;\
    go p = f (P'# p ! 0#) <> go (p +. 1#);\
    end = p +. n}} ;\
instance Modify A X where { ;\
  xs %= f = ieach xs \ i x → write xs i (f x)} ;\
instance Fold S C1 (x ∷ K X) where {;\
    fold (S# s) r0 f = go (P'# s) r0 where {\
      go p r = let ch = p!0#;\
               in if ch == coerce '\0'# then r;\
               else go (p +. 1#) (f r ch)};\
    ifold (S# (P'# → p)) r0 f = go 0# r0 where {\
      go i r = let ch = p!i;\
               in if ch == coerce '\0'# then r;\
               else go (i + 1#) (f i r ch)}};\
instance Fold S C (x ∷ K X) where {;\
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where {go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)};\
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where {go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))}};\
instance FoldIO s S C1 (x ∷ K X) where {;\
    foldIO (S# s) r0 f = go (P'# s) r0 where {;\
      go p r = let ch = p!0#;\
               in if ch == coerce '\0'# then return r;\
               else go (p +. 1#) =<< f r ch};\
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where {\
      go i r = let ch = p!i;\
               in if ch == coerce '\0'# then return r;\
               else go (i + 1#) =<< f i r ch}};\
instance FoldIO s S C (x ∷ K X) where {\
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where {\
      go p r = let C1# ch = p!0#;\
               in if ch == '\0'# then return r;\
               else let n = byteCount ch;\
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)};\
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where {\
      go i r = let C1# ch = p!i;\
               in if ch == '\0'# then return r;\
               else let n = byteCount ch;\
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)}};\
INST_MAP2_UB(X,I) ;\
INST_MAP2_UB(X,I1) ;\
INST_MAP2_UB(X,I2) ;\
INST_MAP2_UB(X,I4) ;\
INST_MAP2_UB(X,I8) ;\
INST_MAP2_UB(X,U) ;\
INST_MAP2_UB(X,U1) ;\
INST_MAP2_UB(X,U2) ;\
INST_MAP2_UB(X,U4) ;\
INST_MAP2_UB(X,U8) ;\
INST_MAP2_UB(X,F4) ;\
INST_MAP2_UB(X,F8) ;\
INST_MAP2_UB(X,Addr#) ;\
instance Foldr A' X where {;\
  foldr v b0 abb = go 0# where {;\
    n = len v;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}};\
instance Foldr P'## X where {;\
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where {;\
    go i = if i < n then abb (v!i) (go (i+1#)) else b0}}

instance Foldr AR' a where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr AR' (a ∷ T_) where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr Ar' a where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr Ar' (a ∷ T_) where
  foldr v b0 abb = go 0# where
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

{-
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
-}

type Modify ∷ ∀ {r} {rv}. (★ → T r → T rv) → T r → TC
class Modify v a where
  (%=) ∷ ∀ s. v s a → (a → a) → ST_ s


-- TODO <%= atomic modify 2 ioref


data ByteCount = One | Two | Three | Four
{-# INLINE byteCount #-}
byteCount ∷ C → ByteCount
byteCount ch
    | ch <= '\x7F'# = One
    | ch <= '\xDF'# = Two
    | ch <= '\xEF'# = Three
    | T             = Four

instance P' C1 +. ByteCount where
  (+.) p = \case {One → p +. 1#; Two → p +. 2#; Three → p +. 3#; Four → p +. 4#}
instance I +. ByteCount where
  (+.) i = \case {One → i + 1#; Two → i + 2#; Three → i + 3#; Four → i + 4#}

-- | Take the current address, read unicode char of the given size.
-- We obviously want the number of bytes, but we have to read one
-- byte to determine the number of bytes for the current codepoint
-- so we might as well reuse it and avoid a read.
--
-- Side Note: We don't dare to decode all 4 possibilities at once.
-- Reading past the end of the addr might trigger an exception.
-- For this reason we really have to check the width first and only
-- decode after.
{-# INLINE unpackUtf8C# #-}
unpackUtf8C# ∷ ByteCount → C → P' C1 → C
unpackUtf8C# bytes ch (coerce @_ @(P' C1) → p) =
  case bytes of
    One   →                     ch
    Two   → cast ( ((cast       ch - 0xC0#) <<#  6##)
                 +  (cast (p ! 1#) - 0x80#          ))
    Three → cast ( ((cast       ch - 0xE0#) <<# 12##)
                 + ((cast (p ! 1#) - 0x80#) <<#  6##)
                 + ( cast (p ! 2#) - 0x80#          ))
    Four  → cast ( ((cast       ch - 0xF0#) <<# 18##)
                 + ((cast (p ! 1#) - 0x80#) <<# 12##)
                 + ((cast (p ! 2#) - 0x80#) <<#  6##)
                 + ( cast (p ! 3#) - 0x80#          ))

instance Each s (A s) I where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' I where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## I where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A I where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K I) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K I) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K I) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K I) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' I where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## I where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' I U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' I I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) I8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' I8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## I8 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A I8 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K I8) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K I8) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K I8) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K I8) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' I8 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## I8 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' I8 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' I8 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I8 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I8 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I8 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I8 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) I1 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' I1 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## I1 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A I1 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K I1) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K I1) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K I1) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K I1) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' I1 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## I1 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' I1 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' I1 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I1 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I1 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I1 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I1 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I1 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I1 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) I2 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' I2 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## I2 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A I2 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K I2) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K I2) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K I2) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K I2) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' I2 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## I2 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' I2 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' I2 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I2 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I2 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I2 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I2 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I2 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I2 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) I4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' I4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## I4 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A I4 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K I4) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K I4) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K I4) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K I4) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' I4 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## I4 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' I4 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' I4 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' I4 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' I4 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' I4 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' I4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) I4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## I4 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) U where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' U where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## U where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A U where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K U) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K U) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K U) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K U) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' U where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## U where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' U U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' U I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) U1 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' U1 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## U1 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A U1 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K U1) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K U1) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K U1) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K U1) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' U1 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## U1 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' U1 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' U1 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U1 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U1 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U1 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U1 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U1 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U1 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) U2 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' U2 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## U2 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A U2 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K U2) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K U2) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K U2) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K U2) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' U2 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## U2 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' U2 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' U2 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U2 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U2 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U2 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U2 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U2 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U2 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) U4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' U4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## U4 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A U4 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K U4) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K U4) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K U4) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K U4) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' U4 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## U4 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' U4 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' U4 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U4 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U4 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U4 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U4 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) U8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' U8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## U8 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A U8 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K U8) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K U8) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K U8) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K U8) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' U8 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## U8 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' U8 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' U8 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' U8 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' U8 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' U8 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' U8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) U8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## U8 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Each s (A s) F4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' F4 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## F4 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A F4 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K F4) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K F4) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K F4) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K F4) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' F4 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## F4 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' F4 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' F4 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F4 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F4 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F4 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F4 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F4 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) F8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' F8 where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## F8 where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A F8 where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K F8) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K F8) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K F8) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K F8) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' F8 where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## F8 where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' F8 U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' F8 I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' F8 Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' F8 Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' F8 Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' F8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) F8 Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## F8 Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Each s (A s) Addr# where  
  each v f = ieach v (\_ → f) 
  ieach v f = lenM v >>* \n → gogo n 0# where  
    gogo n = go where  
        go i | i == n = \s→s 
        go i = v !! i >>* \x → f i x <> go (i-1#)  
instance Each s A' Addr# where  
  each v f = ieach v (\_ → f) 
  ieach v f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (v!i) <> go (i-1#) 
    n = len v 
instance Each s P'## Addr# where  
  ieach (P'_Len# (# p, n #)) f = go 0# where  
    go i | i == n = \s→s 
    go i = f i (P'# p ! i) <> go (i-1#) 
  each (P'_Len# (# p, n #)) f = go p where  
    go p | p == end = \s→s 
    go p = f (P'# p ! 0#) <> go (p +. 1#)
    end = p +. n 
instance Modify A Addr# where  
  xs %= f = ieach xs \ i x → write xs i (f x) 
instance Fold S C1 (x ∷ K Addr#) where 
    fold (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then r
               else go (p +. 1#) (f r ch)
    ifold (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then r
               else go (i + 1#) (f i r ch)
instance Fold S C (x ∷ K Addr#) where 
  fold (S# p0) r0 f = go (P'# @C1 p0) r0 where go p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (p +. n) (r `f` unpackUtf8C# n ch p)
  ifold (S# p0) r0 f = go 0# (P'# @C1 p0) r0 where go i p r = let C1# ch = p!0# in if ch == '\0'# then r else let n = byteCount ch in go (i + 1#) (p +. n) (f i r (unpackUtf8C# n ch p))
instance FoldIO s S C1 (x ∷ K Addr#) where 
    foldIO (S# s) r0 f = go (P'# s) r0 where 
      go p r = let ch = p!0#
               in if ch == coerce '\0'# then return r
               else go (p +. 1#) =<< f r ch
    ifoldIO (S# (P'# → p)) r0 f = go 0# r0 where 
      go i r = let ch = p!i
               in if ch == coerce '\0'# then return r
               else go (i + 1#) =<< f i r ch
instance FoldIO s S C (x ∷ K Addr#) where 
  foldIO (S# p0) r0 io = go (P'# @C1 p0) r0 where 
      go p r = let C1# ch = p!0#
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (p +. n) =<< io r (unpackUtf8C# n ch p)
  ifoldIO (S# (P'# → p)) r0 io = go 0# r0 where 
      go i r = let C1# ch = p!i
               in if ch == '\0'# then return r
               else let n = byteCount ch
                    in go (i +. n) =<< io i r (unpackUtf8C# n ch p)
instance Foldr A' Addr# where 
  foldr v b0 abb = go 0# where 
    n = len v
    go i = if i < n then abb (v!i) (go (i+1#)) else b0
instance Foldr P'## Addr# where 
  foldr (P'_Len# (# P'# → v, n #)) b0 abb = go 0# where 
    go i = if i < n then abb (v!i) (go (i+1#)) else b0

instance Map A' Addr# U where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# U where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# U where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# U where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# U where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# U where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# U1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# U1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# U1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# U1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# U1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# U2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# U2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# U2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# U2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# U2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# U4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# U4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# U4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# U4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# U4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# U8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# U8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# U8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# U8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# U8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# I where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# I where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# I where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# I where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# I where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# I where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 


instance Map A' Addr# I1 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# I1 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# I1 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# I1 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# I1 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# I2 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# I2 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# I2 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# I2 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# I2 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# I4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# I4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# I4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# I4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# I4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# I8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# I8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# I8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# I8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# I8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# F4 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# F4 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# F4 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# F4 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# F4 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# F8 where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# F8 where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# F8 where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# F8 where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# F8 where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 

instance Map A' Addr# Addr# where  
  imap xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f i x) 
    freeze## ys  
  map xs f = runST ST.do  
    ys ← new# (len xs) 
    ieach xs \ i x → write ys i (f x) 
    freeze## ys  
instance MapIO s A' Addr# Addr# where  
  mapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f x s of (# ss, y #) → write ys i y ss) 
    freeze## ys  
  imapIO xs f = ST.do  
    ys ← new# (len xs) 
    ieach xs (\ i x s → case f i x s of (# ss, y #) → write ys i y ss) 
    freeze## ys 
instance Fold A' Addr# Addr# where 
  fold v b0 bab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (bab b (v!i)) else b 
  ifold v b0 ibab = go 0# b0 where 
    n = len v 
    go i b = if i < n then go (i + 1#) (ibab i b (v!i)) else b 
instance FoldIO s A' Addr# Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    n = len v
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
instance FoldIO s (A s) Addr# Addr# where 
  foldIO v b0 bamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← bamb b x
        go (i + 1#) b'
       else return b 
  ifoldIO v b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      n ← lenM v
      if i < n then ST.do 
        x ← v !! i
        b' ← ibamb i b x
        go (i + 1#) b'
       else return b 
instance FoldIO s P'## Addr# Addr# where  
  foldIO (P'_Len# (# P'# → v, n #)) b0 bamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← bamb b (v!i)
        go (i + 1#) b'
       else return b 
  ifoldIO (P'_Len# (# P'# → v, n #)) b0 ibamb = go 0# b0 where 
    go i b = ST.do 
      if i < n then ST.do 
        b' ← ibamb i b (v!i)
        go (i + 1#) b'
      else return b 
