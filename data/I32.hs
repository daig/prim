{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------
-- | Description : 32-bit Integers
--------------------------------------------------------------------
module I32 (I32(I32#,I32,Min,Max), module X) where
import {-# source #-} I as X (I)
import Cast as X
import Num as X

newtype I32 ∷ T_I where I32# ∷ I → I32

-- | Narrow a machine 'I' to 32 bits
pattern I32 ∷ I → I32
pattern I32 i ← (coerce → i) where I32 = cast
{-# complete I32 #-}

deriving newtype instance (≡) I32
deriving newtype instance (≤) I32
instance ℕ I32 where
  (I32 x) + (I32 y) = I32 (x +# y)
  (I32 x) × (I32 y) = I32 (x *# y)
  (I32 x) / (I32 y) = I32 (divInt# x y)
  (I32 x) % (I32 y) = I32 (modInt# x y)
  (I32 x) /% (I32 y) = case x //%% y of (# d, m #) → (# I32 d, I32 m #)
  addC (I32 a) (I32 b) = let c = a + b in (# I32 c , c > coerce Max #)
  subC (I32 a) (I32 b) = let c = a - b in (# I32 c , c < coerce Min #)
instance ℤ I32 where
  negate (I32 x) = I32 (negateInt# x)
  (I32 x) - (I32 y) = I32 (x -# y)
  I32 x // I32 y = I32 (quotInt# x y)
  I32 x %% I32 y = I32 (remInt# x y)
  I32 x //%% I32 y = case quotRemInt# x y of (# q, r #) → (# I32 q, I32 r #)

instance Cast I32 I where cast = coerce narrow32Int#

pattern Max, Min ∷ I32
pattern Max = I32#  0x7FFFFFFF#
pattern Min = I32# -0x80000000#
