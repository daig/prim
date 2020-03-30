module Bool  where
import GHC.Types (isTrue#)

pattern O = B# 0#
pattern I = B# 1#
{-# complete O,I #-}

bool :: B -> a -> a -> a
bool (B# p) a b = if isTrue# p then a else b
