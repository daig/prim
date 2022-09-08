module I where


-- |Add reporting overflow.
addC ∷ a → a → (# a, B #) -- ^ The truncated sum and whether it overflowed
addC = coerce addIntC#
-- |Subtract reporting overflow
subC ∷ a → a → (# a, B #) -- ^ The truncated subtraction and whether it underflowed
subC = coerce subIntC
