{-# language InstanceSigs, CPP #-}
--------------------------------------------------------------------
-- | Description : Effectful Streams
--------------------------------------------------------------------
module Stream where
import Array.Map
import Do.ST as ST
import GHC.Magic
import System.IO (print)
import GHC.Types qualified as GHC

type Stream ∷ ∀ {rr} {ra}. T rr → T ra → T (SumRep '[rr,BoxedRep Lifted])
newtype Stream r a = Stream ((# r | IO (# a, Stream r a #) #))

--sgen ∷ ∀ s r a. (Do2 (# r | (# a, s #) #) (Stream r a), Do2 (Stream r a) (# a, Stream r a #))
--     ⇒ (s → IO (# r | (# a, s #) #)) → s → IO (Stream r a)



-- | Map over the elements of a 'Stream'
type SMap ∷ ∀ {rr} {ra} {rb}. T rr → T ra → T rb → TC
class SMap r a b where
  smap ∷ Stream r a → (a → IO b) → Stream r b
  smap' ∷ Stream r a → (a → b) → Stream r b
  ismap ∷ Stream r a → (I → a → IO b) → Stream r b
  ismap' ∷ Stream r a → (I → a → b) → Stream r b
-- | Map over the termination value of a stream
  rmap ∷ Stream a r → (a → b) → Stream b r
  sgen ∷ (a → IO (# r | (# b, a #) #)) → a → IO (Stream r b)
-- | Lazy right fold using the termination value as seed.
type Fold1 ∷ ∀ {ra}. T ra → TC
class Fold1 a where
  fold1 ∷ Stream r a → (a → r → r) → IO r
  ifold1 ∷ Stream r a → (I → a → r → r) → IO r

type SFold ∷ ∀ {ra} {rb}. T ra → T rb → TC
class SFold a b where
  sfold    ∷ Stream (##) a → b → (    b → a →    b) → IO b
  isfold   ∷ Stream (##) a → b → (I → b → a →    b) → IO b
  sfoldIO  ∷ Stream (##) a → b → (    b → a → IO b) → IO b
  isfoldIO ∷ Stream (##) a → b → (I → b → a → IO b) → IO b

#define INST_SMAP(R,A,B)\
instance SMap R A B where {\
  smap s0 f = go s0 where {\
    go ∷ Stream R A → Stream R B;\
    go (Stream (# r | #)) = Stream (# r | #);\
    go (Stream (# | st #)) = Stream (# | st' #) where {\
      st' t = case st t of {(# tt, (# a, s #) #) → case f a tt of (# ttt, b #) → (# ttt, (# b, go s #) #)}}};\
  ismap s0 f = go 0# s0 where {\
    go ∷ I → Stream R A → Stream R B;\
    go _ (Stream (# r | #)) = Stream (# r | #);\
    go i (Stream (# | st #)) = Stream (# | st' #) where {\
      st' t = case st t of (# tt, (# a, s #) #) → case f i a tt of {(# ttt, b #) → (# ttt, (# b, go (i + 1#) s #) #) } } };\
  smap' s0 f = go s0 where {\
    go ∷ Stream R A → Stream R B;\
    go (Stream (# r | #)) = Stream (# r | #);\
    go (Stream (# | st #)) = Stream (# | st' #) where {\
      st' t = case st t of {(# tt, (# a, s #) #) → (# tt, (# f a, go s #) #)}}};\
  ismap' s0 f = go 0# s0 where {\
    go ∷ I → Stream R A → Stream R B;\
    go _ (Stream (# r | #)) = Stream (# r | #);\
    go i (Stream (# | st #)) = Stream (# | st' #) where {\
      st' t = case st t of {(# tt, (# a, s #) #) → (# tt, (# f i a, go (i + 1#) s #) #)}}};\
  rmap s0 f = go s0 where {\
    go ∷ Stream A R → Stream B R;\
    go (Stream (# r | #)) = Stream (# f r | #);\
    go (Stream (# | st #)) = Stream (# | \t → case st t of (# tt, (# x, s #) #) → (# tt, (# x, go s #) #)  #)};\
    sgen ∷ (A → IO (# R | (# B, A #) #)) → A → IO (Stream R B);\
    sgen step = go where {;\
      go ∷ A → IO (Stream R B);\
      go s = \t → case step s t of {;\
        (# tt, (# r | #) #) → (# tt, Stream (# r | #) #);\
        (# tt, (# | (# a, s #) #) #) → (# tt, Stream (# | \t' → case go s t' of {(# tt', st #) → (# tt', (# a, st #) #)} #)  #)}};\
    }

INST_SMAP((##),I,I)

nums :: _
nums = (`sgen` 0#) \ i → \t → case printI i t of (# tt, () #) → (# t, if i > 10# then (# (##) | #) else (# | (# i, i + 1# #) #) #)

--foo = each nums \ x → \s → s



#define INST_FOLD1(A)\
instance Fold1 A where {;\
  fold1 ∷ ∀ r. Stream r A → (A → r → r) → IO r;\
  fold1 s0 c = go s0 where {;\
    go ∷ Stream r A → IO r;\
    go (Stream (# r | #)) = return r;\
    go (Stream (# | st #)) = interleaveIO# \t → case st t of {(# tt, (# a, s #) #) → case go s tt of {(# ttt, r #) → (# ttt, c a r #)}}};\
  ifold1 ∷ ∀ r. Stream r A → (I → A → r → r) → IO r;\
  ifold1 s0 c = go 0# s0 where {;\
    go ∷ I → Stream r A → IO r;\
    go i (Stream (# r | #)) = return r;\
    go i (Stream (# | st #)) = interleaveIO# \t → case st t of {(# tt, (# a, s #) #) → case go (i + 1#) s tt of {(# ttt, r #) → (# ttt, c i a r #)}}}}

INST_FOLD1(I)
printI i = case print (GHC.I# i) of GHC.IO io → io

#define INST_SFOLD(A,B)\
instance SFold A B where {\
  sfold s0 b0 bab = go b0 s0 where {;\
    go ∷ B → Stream (##) A → IO B;\
    go b (Stream (# (##) | #)) = return b;\
    go b (Stream (# | st #)) = \t → case st t of {(# tt, (# a, s #) #) → go (bab b a) s tt}};\
  isfold s0 b0 ibab = go 0# b0 s0 where {;\
    go ∷ I → B → Stream (##) A → IO B;\
    go i b (Stream (# (##) | #)) = return b;\
    go i b (Stream (# | st #)) = \t → case st t of {(# tt, (# a, s #) #) → go (i + 1#) (ibab i b a) s tt}};\
  sfoldIO s0 b0 bab = go b0 s0 where {;\
    go ∷ B → Stream (##) A → IO B;\
    go b (Stream (# (##) | #)) = return b;\
    go b (Stream (# | st #)) = \t → case st t of {(# tt, (# a, s #) #) → case bab b a tt of {(# ttt, bb #) → go bb s ttt}}};\
  isfoldIO s0 b0 ibab = go 0# b0 s0 where {;\
    go ∷ I → B → Stream (##) A → IO B;\
    go i b (Stream (# (##) | #)) = return b;\
    go i b (Stream (# | st #)) = \t → case st t of {(# tt, (# a, s #) #) → case ibab i b a tt of {(# ttt, bb #) → go (i + 1#) bb s ttt}}}}


#define INST_STREAM3(R,A,B)\
instance Pure b ⇒ Do2 (a ∷ K (# A, Stream R B #)) (b ∷ K (B)) where {;\
  st >% f = st >>= \a → return (f a);\
  f %< st = st >>= \a → return (f a);\
  st >>= f = \s → case st s of {(# ss, a #) → f a ss};\
  f =<< st = \s → case st s of {(# ss, a #) → f a ss}};\
instance Pure b ⇒ Do2 (a ∷ K (# A, Stream R A #)) (b ∷ K (# B, Stream R B #)) where {;\
  st >% f = st >>= \a → return (f a);\
  f %< st = st >>= \a → return (f a);\
  st >>= f = \s → case st s of {(# ss, a #) → f a ss};\
  f =<< st = \s → case st s of {(# ss, a #) → f a ss}};\
INST_SMAP(R,A,B);\


{-
instance FoldIO RealWorld (Stream R) A B where {;\
  foldIO s0 b0 io = go b0 s0 where {;\
    go ∷ B → Stream R A → IO B;\
    go b0 (Stream st) = ST.do {;\
      (# x, s #) ← st;\
      b ← io b0 x;\
      go b s}};\
  ifoldIO s0 b0 io = go 0# b0 s0 where {;\
    go ∷ I → B → Stream R A → IO B;\
    go i b0 (Stream st) = ST.do {;\
      (# x, s #) ← st;\
      b ← io i b0 x;\
      go (i +1#) b s}}}
-}

#define INST_STREAM2(R,A)\
INST_STREAM3(R,A,U);\
INST_STREAM3(R,A,U1);\
INST_STREAM3(R,A,U2);\
INST_STREAM3(R,A,U4);\
INST_STREAM3(R,A,U8);\
INST_STREAM3(R,A,I);\
INST_STREAM3(R,A,I1);\
INST_STREAM3(R,A,I2);\
INST_STREAM3(R,A,I4);\
INST_STREAM3(R,A,I8);\
INST_STREAM3(R,A,F4);\
INST_STREAM3(R,A,F8);\
INST_STREAM3(R,A,P#);\
INST_STREAM3(R,A,ByteArray#);\
INST_STREAM3(R,A,());\
INST_STREAM3(R,A,(##));\
INST_PURE((Stream R A));\
INST_SFOLD(A,R);\
INST_RMAP(R,A)

#define INST_DO2(A,B)\
instance Pure b ⇒ Do2 (a ∷ K A) (b ∷ K B) where {;\
  st >% f = st >>= \a → return (f a);\
  f %< st = st >>= \a → return (f a);\
  st >>= f = \s → case st s of {(# ss, a #) → f a ss};\
  f =<< st = \s → case st s of {(# ss, a #) → f a ss}}


#define INST_DO(A)\
instance Do (a ∷ K A) where {\
  (st >> sta) s = sta (st s) ;\
  st >* f = \s → case st s of {(# ss, _ #) → f ss};\
  st >>* f = \s → case st s of {(# ss, a #) → f a ss};\
  f *<< st = \s → case st s of {(# ss, a #) → f a ss}}

#define INST_PURE(A)\
instance Pure A where {pure a = \s → (# s, a #)}
{-
-}

#define INST_STREAM(R)\
INST_STREAM2(R,U);\
INST_STREAM2(R,U1);\
INST_STREAM2(R,U2);\
INST_STREAM2(R,U4);\
INST_STREAM2(R,U8);\
INST_STREAM2(R,I);\
INST_STREAM2(R,I1);\
INST_STREAM2(R,I2);\
INST_STREAM2(R,I4);\
INST_STREAM2(R,I8);\
INST_STREAM2(R,F4);\
INST_STREAM2(R,F8);\
INST_STREAM2(R,P#);\
INST_STREAM2(R,ByteArray#);\
INST_STREAM2(R,());\
INST_STREAM2(R,(##));\
INST_FOLD1(R);\
INST_EACH(R)

#define INST_EACH(R)\
instance Each RealWorld (Stream (##)) R where {;\
  each s0 io = go s0 where {;\
    go ∷ Stream (##) R → IO_;\
    go (Stream st) = case st of {(# (##) | #) → \s→s; (# | st #) → st >>* \(# x, s #) → io x <> go s}};\
  ieach s0 io = go 0# s0 where {;\
    go ∷ I → Stream (##) R → IO_;\
    go i (Stream st) = case st of {(# (##) | #) → \s→s; (# | st #) → st >>* \(# x, s #) → io i x <> go (i + 1#) s}}}

INST_EACH(I)


{-
INST_STREAM((##))
INST_STREAM(())
INST_STREAM(U)
INST_STREAM(U1)
INST_STREAM(U2)
INST_STREAM(U4)
INST_STREAM(U8)
INST_STREAM(I)
INST_STREAM(I1)
INST_STREAM(I2)
INST_STREAM(I4)
INST_STREAM(I8)
INST_STREAM(F4)
INST_STREAM(F8)
INST_STREAM(P#)
INST_STREAM(ByteArray#)
-}
