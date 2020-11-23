module P.Block where

type P = MVar#

(≡), eq ∷ P s a → P s a → B#
(≡) = sameMVar#; eq = sameMVar#

empty' ∷ P s a → ST# s B#
empty' = isEmptyMVar#

-- | A new empty @P@
new ∷ ST# s (P s a)
new = newMVar#

-- | Block until the @P@ is full/unlocked, then atomically take the value and lock it.
take ∷ P s a → ST# s a
take = takeMVar#

-- | Take the current value if it exists and lock the @P@
take' ∷ P s a → ST# s (Maybe# a) {- ^ The value if the @P@ was full/unlocked -}
take' r s0 = case tryTakeMVar# r s0 of
  (# s1, full', a #) → (# s1, (# full', a #) #)

-- | Block until the @P@ is full/unlocked, then read the value but don't lock it.
read ∷ P s a → ST# s a
read = readMVar#

-- | Take the current value if it exists but don't lock the @P@
read' ∷ P s a → ST# s (Maybe# a) {- ^ The value if the @P@ was full/unlocked -}
read' r s0 = case tryReadMVar# r s0 of
  (# s1, full', a #) → (# s1, (# full', a #) #)

write ∷ P s a → a → ST_# s
write = putMVar#

write' ∷ P s a → a → ST# s B# {- ^ whether the write succeeded -}
write' = tryPutMVar#
