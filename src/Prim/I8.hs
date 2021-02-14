--------------------------------------------------------------------
-- | Description : 8-bit Signed Integer operations
--------------------------------------------------------------------
{-# language CPP #-}
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module Prim.I8 (I8(I8,I8), module Prim.I8) where
import Prim.I ()

newtype I8  ∷ T_I where I8  ∷ I → I8
-- | Narrow a machine 'I' to 8 bits
pattern I8 ∷ I → I8
pattern I8 i ← (coerce → i) where I8 = coerce narrow8Int#
{-# complete I8 #-}


deriving newtype instance (≡) I8
deriving newtype instance (≤) I8
instance ℕ I8 where
  (I8 x) + (I8 y) = I8 (x +# y)
  (I8 x) × (I8 y) = I8 (x *# y)
  (I8 x) / (I8 y) = I8 (divInt# x y)
  (I8 x) % (I8 y) = I8 (modInt# x y)
  (I8 x) /% (I8 y) = case x //%% y of (# d, m #) → (# I8 d, I8 m #)
  addC (I8 a) (I8 b) = let c = a + b in (# I8 c , c > coerce Max #)
  subC (I8 a) (I8 b) = let c = a - b in (# I8 c , c < coerce Min #)

instance ℤ I8 where
  negate (I8 x) = I8 (negateInt# x)
  (I8 x) - (I8 y) = I8 (x -# y)
  I8 x // I8 y = I8 (quotInt# x y)
  I8 x %% I8 y = I8 (remInt# x y)
  I8 x //%% I8 y = case quotRemInt# x y of (# q, r #) → (# I8 q, I8 r #)

pattern Max, Min ∷ I8
--pattern Max =  I8 INT_MAX#
pattern Max =  I8 0x7F#
pattern Min = I8 -0x80#

