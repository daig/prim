module Char (module Char, B) where

(≡), (≠) ∷ Char → Char → B
(≡) = eqChar
(≠) = neChar
{-# inline (≡) #-}
{-# inline (≠) #-}
