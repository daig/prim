{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Num where
import Cmp
import Bits
import Cast
import HsFFI
import GHC.Word (Word(..))
import GHC.Int (Int(..))

-- |Satisfies @((((x / y) × y) + (x % y) ≡ x@. The
class (≤) a ⇒ ℕ (a ∷ T r) where
  (+), (×) ∷ a → a → a
  -- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
  (/), (%) ∷ a {- ^ dividend -}  → a {- ^ divisor -} → a
  -- | Satisfies @((x / y) + ((x % y) × y) ≡ x@.
  (/%) ∷ a → a → (# a , a #)
class ℕ a ⇒ ℤ (a ∷ T r) where
  -- |Satisfies @((((x // y) × y) + (x %% y) ≡ x@.
  (//),(%%) ∷ a → a → a
  -- | Rounds towards 0. The behavior is undefined if the first argument is zero.
  (//%%) ∷ a → a → (# a , a #)
  (-) ∷ a → a → a
  negate ∷ a → a
class ℤ a ⇒ ℝ (a ∷ T r) where
  abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ a → a
  (**) ∷ a → a → a

instance ℕ U where
  (+) = plusWord#
  (×) = timesWord#
  (/) = quotWord#
  (%) = remWord#
  (/%) = quotRemWord#

instance ℕ U8 where
  (+) = plusWord8#
  (×) = timesWord8#
  (/) = quotWord8#
  (%) = remWord8#
  (/%) = quotRemWord8#

instance ℕ U16 where
  (+) = plusWord16#
  (×) = timesWord16#
  (/) = quotWord16#
  (%) = remWord16#
  (/%) = quotRemWord16#

instance ℕ U32 where
  (+) = plusWord32#
  (×) = timesWord32#
  (/) = quotWord32#
  (%) = remWord32#
  (/%) = quotRemWord32#

instance ℕ U64 where
  (+) = plusWord64#
  (×) = timesWord64#
  (/) = quotWord64#
  (%) = remWord64#
  x /% y = (# x / y, x % y #)

-- | Low word of signed integer multiply
-- 
-- Modular functions have built-in rules.
instance ℕ I where
  (+) = (+#)
  (×) = (*#)
  (%) = modInt#
  (/) = divInt#
  x /% y = case 0# < x ∧ 0# > y of
      T → case (x - 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
      F → case 0# > x ∧ 0# < y of
        T → case (x + 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
        F → x //%% y
instance ℤ I where
  negate = negateInt#
  (-) = (-#)
  (//) = quotInt#
  (%%) = remInt#
  (//%%) = quotRemInt#


instance ℕ I8 where
  (+) = plusInt8#
  (×) = timesInt8#
  (/) = quotInt8#
  (%) = remInt8#
  x /% y = (# x / y, x % y #)

instance ℕ I16 where
  (+) = plusInt16#
  (×) = timesInt16#
  (/) = quotInt16#
  (%) = remInt16#
  x /% y = (# x / y, x % y #)
instance ℕ I32 where
  (+) = plusInt32#
  (×) = timesInt32#
  (/) = quotInt32#
  (%) = remInt32#
  x /% y = (# x / y, x % y #)
instance ℕ I64 where
  (+) = plusInt64#
  (×) = timesInt64#
  (/) = quotInt64#
  (%) = remInt64#
  x /% y = (# x / y, x % y #)

instance ℤ I8 where
  negate = negateInt8#
  (-) = subInt8# 
  (//) = quotInt8#
  (%%) = remInt8#
  (//%%) = quotRemInt8#

instance ℤ I16 where
  negate = negateInt16#
  (-) = subInt16# 
  (//) = quotInt16#
  (%%) = remInt16#
  (//%%) = quotRemInt16#
instance ℤ I32 where
  negate = negateInt32#
  (-) = subInt32# 
  (//) = quotInt32#
  (%%) = remInt32#
  (//%%) = quotRemInt32#
instance ℤ I64 where
  negate = negateInt64#
  (-) = subInt64# 
  (//) = quotInt64#
  (%%) = remInt64#
  (cast -> a) //%% (cast -> b) =
    case quotRemInt# a b of (# q, r #) -> (# cast q, cast r #)

instance ℕ F32 where
  (+) = plusFloat#
  (×) = timesFloat#
  (/) = divideFloat#
  _ % _ = 0.0#
  x /% y = (# x / y , 0.0# #)
instance ℤ F32 where
  negate = negateFloat#
  (-) = minusFloat#
  (//) = divideFloat#
  _ %% _ = 0.0#
  x //%% y = (# x / y , 0.0# #)
instance ℝ F32 where
  abs = fabsFloat#
  exp = expFloat#
  log = logFloat#
  sqrt = sqrtFloat#
  sin = sinFloat#
  cos = cosFloat#
  tan = tanFloat#
  asin = asinFloat#
  acos = acosFloat#
  atan = atanFloat#
  sinh = sinhFloat#
  cosh = coshFloat#
  tanh = tanhFloat#
  (**) = powerFloat#
instance ℕ F64 where
  (+) = (+##)
  (×) = (*##)
  (/) = (/##)
  _ % _ = 0.0##
  x /% y = (# x / y , 0.0## #)
instance ℤ F64 where
  negate = negateDouble#
  (-) = (-##)
  (//) = (/##)
  _ %% _ = 0.0##
  x //%% y = (# x / y , 0.0## #)
instance ℝ F64 where
  abs = fabsDouble#
  exp = expDouble#
  log = logDouble#
  sqrt = sqrtDouble#
  sin = sinDouble#
  cos = cosDouble#
  tan = tanDouble#
  asin = asinDouble#
  acos = acosDouble#
  atan = atanDouble#
  sinh = sinhDouble#
  cosh = coshDouble#
  tanh = tanhDouble#
  (**) = (**##)
