--------------------------------------------------------------------
-- | Description : 31-bit Unicode code points
--------------------------------------------------------------------
module Char (Char, pattern Char ,module X) where
import {-# source #-} I
import Cmp as X
import Cast as X

-- | 31-bit Unicode code points
type Char = Char#


pattern Char ∷ I → Char
pattern Char{toI} ← (ord# → toI) where Char = chr#
