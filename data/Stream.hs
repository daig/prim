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

type Stream ∷ ∀ {rr} {ra}. T rr → T ra → ★
newtype Stream r a = Stream (IO (# r | (# a, Stream r a #) #))

--sgen ∷ ∀ s r a. (Do2 (# r | (# a, s #) #) (Stream r a), Do2 (Stream r a) (# a, Stream r a #))
--     ⇒ (s → IO (# r | (# a, s #) #)) → s → IO (Stream r a)

-- | Map over the elements of a 'Stream'
type SMap ∷ ∀ {rr} {ra} {rb}. T rr → T ra → T rb → TC
class SMap r a b where
  -- | effectfully map over the values of a stream
  smap ∷ Stream r a → (a → IO b) → Stream r b
  -- | map over the values of a stream
  smap' ∷ Stream r a → (a → b) → Stream r b
  -- | effectfully map over the values of a stream with access to the index
  ismap ∷ Stream r a → (I → a → IO b) → Stream r b
  -- | map over the values of a stream with access to the index
  ismap' ∷ Stream r a → (I → a → b) → Stream r b
-- | Map over the termination value of a stream
  rmap ∷ Stream a r → (a → b) → Stream b r
  -- | Generate a stream by iterating an effectful action on a seed
  sgen ∷ (a → IO (# r | (# b, a #) #)) → a → Stream r b
  -- | Map over a stream or halt early
  stakeMap' ∷ Stream r a → (a → (# r | b #)) → Stream r b
  -- | Map over a stream or skip elements
  sfilterMap' ∷ Stream r a → (a → (# (##) | b #)) → Stream r b

-- | Lazy right fold using the termination value as seed.
type Fold1 ∷ ∀ {ra}. T ra → TC
class Fold1 a where
  -- | lazily fold over a stream, using the termination value as the base.
  -- eg to build a list
  fold1 ∷ Stream r a → (a → r → r) → IO r
  -- | lazily fold over a stream with access to the index, using the termination value as the base.
  -- eg to build a list
  ifold1 ∷ Stream r a → (I → a → r → r) → IO r
  -- | perform an IO action for each element of the stream
  seach ∷ Stream (##) a → (a → IO_) → IO_
  -- | perform an IO action with access to the index for each element of the stream
  iseach ∷ Stream (##) a → (I → a → IO_) → IO_
  -- | take from the front of a stream
  stake' ∷ I → Stream (##) a → Stream (##) a

type SFold ∷ ∀ {ra} {rb}. T ra → T rb → TC
class SFold a b where
  sfold    ∷ Stream (##) a → b → (    b → a →    b) → IO b
  isfold   ∷ Stream (##) a → b → (I → b → a →    b) → IO b
  sfoldIO  ∷ Stream (##) a → b → (    b → a → IO b) → IO b
  isfoldIO ∷ Stream (##) a → b → (I → b → a → IO b) → IO b
  sfilter  ∷ Stream a b → (b → Bool) → Stream a b
  -- | Halt a stream early when the condition is met
  stakeWhile ∷ Stream a b → (b → (# (##) | a #)) → Stream a b
  -- | Halt with a default value after taking some number of elements.
  stake ∷ I → a → Stream a b → Stream a b

#define INST_SMAP(R,A,B)\
instance SMap R A B where {\
  smap' s0 f = go s0 where {\
    go ∷ Stream R A → Stream R B;\
    go (Stream s0) = Stream \ t → case s0 t of {(# tt, st #) → (# tt, case st of {(# r | #) → (# r | #); (# | (# a, s #) #) → (# | (# f a, go s #) #)} #)}};\
  smap s0 f = go s0 where {;\
    go ∷ Stream (##) A → Stream (##) B;\
    go (Stream s0) = Stream \ t → case s0 t of {(# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# a, s #) #) → case f a tt of {(# ttt, b #) → (# ttt, (# | (# b, go s #) #) #)}}}};\
  ismap' s0 f = go 0# s0 where {\
    go ∷ I → Stream R A → Stream R B;\
    go i (Stream s0) = Stream \ t → case s0 t of {(# tt, st #) → (# tt, case st of {(# r | #) → (# r | #); (# | (# a, s #) #) → (# | (# f i a, go (i + 1#) s #) #)} #)}};\
  ismap s0 f = go 0# s0 where {;\
    go ∷ I → Stream (##) A → Stream (##) B;\
    go i (Stream s0) = Stream \ t → case s0 t of {(# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# a, s #) #) → case f i a tt of {(# ttt, b #) → (# ttt, (# | (# b, go (i + 1#) s #) #) #)}}}};\
  rmap s0 f = go s0 where {\
    go ∷ Stream A R → Stream B R;\
    go (Stream s) = Stream \ t → case s t of (# tt, st #) → (# tt, case st of {(# r | #) → (# f r | #); (# | (# x, ss #) #) → (# | (# x, go ss #) #)} #)};\
  sgen ∷ (A → IO (# R | (# B, A #) #)) → A → Stream R B;\
  sgen step = go where {;\
    go ∷ A → Stream R B;\
    go i = Stream \ t → case step i t of {(# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# x, ss #) #) → (# tt, (# | (# x, go ss #) #) #)}}};\
  stakeMap' s0 arb = go s0 where {;\
    go ∷ Stream R A → Stream R B;\
    go (Stream s) = Stream \ t → case s t of (# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# a, ss #) #) → (# tt, case arb a of {(# r | #) → (# r | #); (# | b #) → (# | (# b, go ss #) #)} #)}};\
  sfilterMap' s0 amb = go s0 where {;\
    go ∷ Stream R A → Stream R B;\
    go (Stream s) = Stream \ t → case s t of (# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# a, ss #) #) → case amb a of {(# | b #) → (# tt, (# | (# b, go ss #) #) #); (# (##) | #) → case go ss of Stream sss → sss tt}}};\
    }

INST_SMAP((##),I,I)

nums ∷ Stream (##) I
nums = (`sgen` 0#) \ i → \t → case printI i t of tt → (# tt, if i > 10# then (# (##) | #) else (# | (# i, i + 1# #) #) #)

allnums ∷ Stream (##) I
allnums = (`sgen` 0#) \ i → \t → (# t, (# | (# i, i + 1# #) #) #)


{-
                                                       -}
--    go ∷ I → Stream r U → IO r
    {-
    go i (Stream (# r | #)) = return r
    go i (Stream (# | st #)) = interleaveIO# \t → case st t of {(# tt, (# a, s #) #) → case go (i + 1#) s tt of {(# ttt, r #) → (# ttt, c i a r #)}}}}
    -}

#define INST_FOLD1(A)\
instance Fold1 A where {;\
  fold1 ∷ ∀ r. Stream r A → (A → r → r) → IO r;\
  fold1 s0 c = go s0 where {;\
    go ∷ Stream r A → IO r;\
    go (Stream s) = interleaveIO# \t → case s t of (# tt, st #) → case st of {\
                                                     (# r | #) → (# tt, r #);\
                                                     (# | (# a, ss #) #) → case go ss tt of (# ttt, r #) → (# ttt, c a r #)}};\
  ifold1 ∷ ∀ r. Stream r A → (I → A → r → r) → IO r;\
  ifold1 s0 c = go 0# s0 where {;\
    go ∷ I → Stream r A → IO r;\
    go i (Stream s) = interleaveIO# \t → case s t of (# tt, st #) → case st of {;\
                                                       (# r | #) → (# tt, r #);\
                                                       (# | (# a, ss #) #) → case go (i + 1#) ss tt of (# ttt, r #) → (# ttt, c i a r #)}};\
  seach s0 io = go s0 where {;\
    go ∷ Stream (##) A → IO_;\
    go (Stream st) = \t → case st t of {(# tt, s #) → case s of {(# (##) | #) → tt; (# | (# x, ss #) #) → case io x tt of ttt → go ss ttt}}};\
  iseach s0 io = go 0# s0 where {;\
    go ∷ I → Stream (##) A → IO_;\
    go i (Stream st) = \t → case st t of {(# tt, s #) → case s of {(# (##) | #) → tt; (# | (# x, ss #) #) → case io i x tt of ttt → go i ss ttt}}};\
  stake' n s0 = go 0# s0 where {;\
    go ∷ I → Stream (##) A → Stream (##) A;\
    go i (Stream s) = Stream \ t → if i == n then (# t, (# (##) | #) #) else case s t of {(# tt, st #) → case st of {(# (##) | #) → (# tt, (# (##) | #) #); (# | (# a, ss #) #) → (# tt, (# | (# a, go (i + 1#) ss #) #) #) }}};\
    }

INST_FOLD1(I)
printI i = case print (GHC.I# i) of GHC.IO io → \t → case io t of (# tt, _ #) → tt

#define INST_SFOLD(A,B)\
instance SFold A B where {\
  sfold s0 b0 bab = go b0 s0 where {;\
    go ∷ B → Stream (##) A → IO B;\
    go b (Stream s) = \t → case s t of {(# tt, st #) → case st of {(# r | #) → (# tt, b #); (# | (# a, ss #) #) → go (bab b a) ss tt}}};\
  isfold s0 b0 ibab = go 0# b0 s0 where {;\
    go ∷ I → B → Stream (##) A → IO B;\
    go i b (Stream s) = \t → case s t of {(# tt, st #) → case st of {(# r | #) → (# tt, b #); (# | (# a, ss #) #) → go (i + 1#) (ibab i b a) ss tt}}};\
  sfoldIO s0 b0 bab = go b0 s0 where {;\
    go ∷ B → Stream (##) A → IO B;\
    go b (Stream s) = \t → case s t of {(# tt, st #) → case st of {(# r | #) → (# tt, b #); (# | (# a, ss #) #) → case bab b a tt of {(# ttt, bb #) → go bb ss tt}}}};\
  isfoldIO s0 b0 ibab = go 0# b0 s0 where {;\
    go ∷ I → B → Stream (##) A → IO B;\
    go i b (Stream s) = \t → case s t of {(# tt, st #) → case st of {(# r | #) → (# tt, b #); (# | (# a, ss #) #) → case ibab i b a tt of {(# ttt, bb #) → go i bb ss tt}}}};\
  sfilter s0 p = go s0 where {;\
    go ∷ Stream A B → Stream A B;\
    go (Stream s) = Stream \ t → case s t of (# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# b, ss #) #) → if p b then (# tt, (# | (# b, go ss #) #) #) else case go ss of Stream sss → sss tt}};\
  stakeWhile s0 p = go s0 where {;\
    go ∷ Stream A B → Stream A B;\
    go (Stream s) = Stream \ t → case s t of (# tt, st #) → case st of {(# r | #) → (# tt, (# r | #) #); (# | (# b, ss #) #) → (# tt, case p b of {(# (##) | #) → (# | (# b, go ss #) #); (# | r #) → (# r | #)} #)}};\
  stake n r s0 = go 0# s0 where {;\
    go ∷ I → Stream A B → Stream A B;\
    go i (Stream s) = Stream \ t → if i == n then (# t, (# r | #) #) else case s t of {(# tt, st #) → case st of {(# rr | #) → (# tt, (# rr | #) #); (# | (# a, ss #) #) → (# tt, (# | (# a, go (i + 1#) ss #) #) #) }}};\
    }

INST_SFOLD(I,U)

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
