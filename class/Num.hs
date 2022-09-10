{-# LANGUAGE StandaloneKindSignatures #-}
module Num (module Num
-- * Note: divInt# implementation
-- | @divInt#@ (truncated toward zero, defined in "GHC.Classes") is implemented with quotInt# (truncated
-- toward negative infinity, defined in "GHC.Prim"). They differ when inputs x and y have different signs:
--
--  - @x `rem` y@ has the sign of @x@ and @(x `quot` y)*y + (x `rem` y) == x@
--  - @x `mod` y@ has the sign of @y@ and @(x `div`  y)*y + (x `mod` y) == x@
--
-- So we bias the input and the result of @quotInt@ as follows:
--
--  @
--         if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
--    else if isTrue# (x# <# 0#) && isTrue# (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
--    else x# `quotInt#` y#
--  @
--
-- However this leads to assembly code with lots of branches (#19636) while we
-- would like simpler code that we could inline (#18067). So we use some
-- branchless code instead as derived below:
--
--  @
--         if isTrue# (x# ># 0#) && isTrue# (y# <# 0#) then ((x# -# 1#) `quotInt#` y#) -# 1#
--    else if isTrue# (x# <# 0#) && isTrue# (y# ># 0#) then ((x# +# 1#) `quotInt#` y#) -# 1#
--    else x# `quotInt#` y#
--  @
--
--  ===> { Give names to constants and always use them }
--
--  @
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        (bias#,hard#)
--          | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) = (-1#, 1#)
--          | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) = ( 1#, 1#)
--          | otherwise                                = ( 0#, 0#)
--  @
--
--  ===> { Compute bias# and hard# independently using Bool# (0#,1#) }
--
-- @
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        c0#   = (x# <# 0#) &&# (y# ># 0#)
--        c1#   = (x# ># 0#) &&# (y# <# 0#)
--        bias# = c0# -# c1#  -- both cases are mutually exclusive so we can subtract them
--        hard# = c0# ||# c1# -- (we could add them too here but OR is slightly better)
-- @
--
--  ===> { Use yn# variable for "y# <# 0#" }
--
-- @
--    ((x# +# bias#) `quotInt#` y#) -# hard#
--      where
--        -- y# ==# 0# throws an exception so we don't need to consider it
--        yn#   = y# <# 0#
--        c0#   = (x# <# 0#) &&# (notI# yn#)
--        c1#   = (x# ># 0#) &&# yn#
--        bias# = c0# -# c1#
--        hard# = c0# ||# c1#
-- @
--
--
-- Note that we need to be careful NOT to overflow if we do any additional
-- arithmetic on the arguments...  the following previous version of this code
-- had problems with overflow:
--
-- @
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
-- @
           ) where
import Cmp
import Bits
import Cast
import HsFFI
import GHC.Word (Word(..))
import GHC.Int (Int(..))

-- |Satisfies @((((x / y) × y) + (x % y) ≡ x@. The
class (≤) a ⇒ ℕ (a ∷ T r) where
  (+), (×) ∷ a → a → a
  -- | Division rounding towards -∞. The behavior is undefined if the first argument is zero.
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
  (/%) = divModInt#
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
