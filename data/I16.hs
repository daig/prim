{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------
-- | Description : 16-bit Integers
--------------------------------------------------------------------
module I16 (I16(I16#,I16,Min,Max),module X) where
import {-# source #-} I as X (I)
import Class as X hiding ((-#),Min,Max)

newtype I16 ∷ T_I where I16# ∷ I → I16

-- | Narrow a machine 'I' to 16 bits
pattern I16 ∷ I → I16
pattern I16 i ← (coerce → i) where I16 = cast
{-# complete I16 #-}

deriving newtype instance (≡) I16
deriving newtype instance (≤) I16

instance ℕ I16 where
  (I16 x) + (I16 y) = I16 (x +# y)
  (I16 x) × (I16 y) = I16 (x *# y)
  (I16 x) / (I16 y) = I16 (divInt# x y)
  (I16 x) % (I16 y) = I16 (modInt# x y)
  (I16 x) /% (I16 y) = case x //%% y of (# d, m #) → (# I16 d, I16 m #)
  addC (I16 a) (I16 b) = let c = a + b in (# I16 c , c > coerce Max #)
  subC (I16 a) (I16 b) = let c = a - b in (# I16 c , c < coerce Min #)
instance ℤ I16 where
  negate (I16 x) = I16 (negateInt# x)
  (I16 x) - (I16 y) = I16 (x -# y)
  I16 x // I16 y = I16 (quotInt# x y)
  I16 x %% I16 y = I16 (remInt# x y)
  I16 x //%% I16 y = case quotRemInt# x y of (# q, r #) → (# I16 q, I16 r #)

pattern Max, Min ∷ I16
pattern Max = I16#  0x7FFF#
pattern Min = I16# -0x800#

instance Cast I16 I where cast = coerce narrow16Int#
