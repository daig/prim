module Ref.Lock where

type Ref = MVar#

eq ∷ Ref s a → Ref s a → B
eq = sameMVar#

empty' ∷ Ref s a → ST s B
empty' = isEmptyMVar#

-- | A new empty @Ref@
new ∷ ST s (Ref s a)
new = newMVar#

-- | Block until the @Ref@ is full/unlocked, then atomically take the value and lock it.
take ∷ Ref s a → ST s a
take = takeMVar#

-- | Take the current value if it exists and lock the @Ref@
take' ∷ Ref s a → ST s (Maybe# a) {- ^ The value if the @Ref@ was full/unlocked -}
take' r s0 = case tryTakeMVar# r s0 of
  (# s1, full', a #) → (# s1, (# full', a #) #)

-- | Block until the @Ref@ is full/unlocked, then read the value but don't lock it.
read ∷ Ref s a → ST s a
read = readMVar#

-- | Take the current value if it exists but don't lock the @Ref@
read' ∷ Ref s a → ST s (Maybe# a) {- ^ The value if the @Ref@ was full/unlocked -}
read' r s0 = case tryReadMVar# r s0 of
  (# s1, full', a #) → (# s1, (# full', a #) #)

write ∷ Ref s a → a → ST_ s
write = putMVar#

write' ∷ Ref s a → a → ST s B {- ^ whether the write succeeded -}
write' = tryPutMVar#
