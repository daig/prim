module Char8 (module Char8, B) where

newtype Char8 = Char8# Char

(≡), (≠) ∷ Char → Char → B
(≡) = coerce eqChar
(≠) = coerce neChar
{-# inline (≡) #-}
{-# inline (≠) #-}
