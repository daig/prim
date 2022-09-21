{-# OPTIONS -Wno-missing-methods #-}
{-# LANGUAGE BangPatterns, CPP  #-}
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
import GHC.Types qualified as GHC (isTrue#)
import GHC.Prim.Exception qualified as GHC
#include "MachDeps.h"

infixl 6 +, -, -?
infixl 7 ×, /, //
infixl 7 %, /%, %%, //%%

-- |Satisfies @((((x / y) × y) + (x % y) ≡ x@. The
class (≤) a ⇒ ℕ (a ∷ T r) where
  (+), (×) ∷ a → a → a
  -- | Subtract without checking overflow
  (-) ∷ a → a → a
  -- | Try to subtract if not overflow.
  -- _Left_ if no overflow. _Right_ if overflow.
  (-?) ∷ a → a → (# a | a #)
  -- | Division rounding towards -∞. The behavior is undefined if the first argument is zero.
  (/), (%) ∷ a {- ^ dividend -}  → a {- ^ divisor -} → a
  -- | Satisfies @((x / y) + ((x % y) × y) ≡ x@.
  (/%) ∷ a → a → (# a , a #)
class ℕ a ⇒ ℤ (a ∷ T r) where
  -- |Satisfies @((((x // y) × y) + (x %% y) ≡ x@.
  (//),(%%) ∷ a → a → a
  -- | Rounds towards 0. The behavior is undefined if the first argument is zero.
  (//%%) ∷ a → a → (# a , a #)
  negate ∷ a → a
  -- | Absolute value
  abs ∷ a → a
  -- | Compare to zero
  sgn ∷ a → Ordering
class 𝕌 (a ∷ T r) where
  -- | Log base 2
  log2 ∷ a → a
  -- | Log in an arbitrary base
  log# ∷ a → a → a
class ℤ a ⇒ ℝ (a ∷ T r) where
  exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ a → a
  -- | @exp x - 1@ but with greater precision for small values of @x@.
  -- Inverse of 'log1p'
  expm1 ∷ a → a
  -- | @log (x + 1)@ but with greater precision for small values of @x@ i.e. when @1 + x ≡ x@.
  -- Inverse of 'expm1'
  log1p ∷ a → a
  (**) ∷ a → a → a

instance ℕ U where
  (+) = plusWord#
  (-) = minusWord#
  (×) = timesWord#
  a -? b = case subWordC# a b of (# u, oflo #) → cast (# B# oflo, u #)
  (/) = quotWord#
  (%) = remWord#
  (/%) = quotRemWord#

instance ℕ U8 where
  (+) = plusWord8#
  (-) = subWord8#
  a -? b = cast (# a > b, a - b #)
  (×) = timesWord8#
  (/) = quotWord8#
  (%) = remWord8#
  (/%) = quotRemWord8#

instance ℕ U16 where
  (+) = plusWord16#
  (-) = subWord16#
  a -? b = cast (# a > b, a - b #)
  (×) = timesWord16#
  (/) = quotWord16#
  (%) = remWord16#
  (/%) = quotRemWord16#

instance ℕ U32 where
  (+) = plusWord32#
  (-) = subWord32#
  a -? b = cast (# a > b, a - b #)
  (×) = timesWord32#
  (/) = quotWord32#
  (%) = remWord32#
  (/%) = quotRemWord32#

instance ℕ U64 where
  (+) = plusWord64#
  (-) = subWord64#
  a -? b = case subWordC# (cast a) (cast b) of (# u, oflo #) → cast (# B# oflo, cast @U64 u #)
  (×) = timesWord64#
  (/) = quotWord64#
  (%) = remWord64#
  x /% y = (# x / y, x % y #)

-- | Low word of signed integer multiply
-- 
-- Modular functions have built-in rules.
instance ℕ I where
  (+) = (+#)
  (-) = (-#)
  a -? b = case subIntC# a b of (# i, oflo #) → cast (# B# oflo, i #)
  (×) = (*#)
  (%) = modInt#
  (/) = divInt#
  (/%) = divModInt#
instance ℤ I where
  negate = negateInt#
  abs i = (i ⊕ nsign) -# nsign where
    nsign = i >># minusWord# WORD_SIZE_IN_BITS## 1##
  (//) = quotInt#
  (%%) = remInt#
  (//%%) = quotRemInt#
  sgn a = Ordering# (coerce (a ># 0#) -# (a <# 0#))


instance ℕ I8 where
  (+) = plusInt8#
  (-) = subInt8#
  (×) = timesInt8#
  (/) = quotInt8#
  (%) = remInt8#
  x /% y = (# x / y, x % y #)

instance ℕ I16 where
  (+) = plusInt16#
  (-) = subInt16#
  (×) = timesInt16#
  (/) = quotInt16#
  (%) = remInt16#
  x /% y = (# x / y, x % y #)
instance ℕ I32 where
  (+) = plusInt32#
  (×) = timesInt32#
  (-) = subInt32#
  (/) = quotInt32#
  (%) = remInt32#
  x /% y = (# x / y, x % y #)
instance ℕ I64 where
  (+) = plusInt64#
  (×) = timesInt64#
  (-) = subInt64#
  a -? b = case subIntC# (cast a) (cast b) of (# i, oflo #) → cast (# B# oflo, cast @I64 i #)
  (/) = quotInt64#
  (%) = remInt64#
  x /% y = (# x / y, x % y #)

instance ℤ I8 where
  negate = negateInt8#
  (//) = quotInt8#
  (%%) = remInt8#
  (//%%) = quotRemInt8#
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))
  abs i = (i ⊕ nsign) - nsign where nsign = i >># 7##

instance ℤ I16 where
  negate = negateInt16#
  (//) = quotInt16#
  (%%) = remInt16#
  (//%%) = quotRemInt16#
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))
  abs i = (i ⊕ nsign) - nsign where nsign = i >># 15##
instance ℤ I32 where
  negate = negateInt32#
  (//) = quotInt32#
  (%%) = remInt32#
  (//%%) = quotRemInt32#
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))
  abs i = (i ⊕ nsign) - nsign where nsign = i >># 31##
instance ℤ I64 where
  negate = negateInt64#
  (//) = quotInt64#
  (%%) = remInt64#
  (cast -> a) //%% (cast -> b) =
    case quotRemInt# a b of (# q, r #) -> (# cast q, cast r #)
  abs i = (i ⊕ nsign) - nsign where nsign = i >># 63##
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))

instance ℕ F32 where
  (+) = plusFloat#
  (-) = minusFloat#
  (×) = timesFloat#
  (/) = divideFloat#
  _ % _ = 0.0#
  x /% y = (# x / y , 0.0# #)
instance ℤ F32 where
  negate = negateFloat#
  (//) = divideFloat#
  _ %% _ = 0.0#
  x //%% y = (# x / y , 0.0# #)
  abs = fabsFloat#
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))
instance ℝ F32 where
  exp = expFloat#
  expm1 = expm1Float#
  log = logFloat#
  log1p = log1pFloat#
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
  (-) = (-##)
  (×) = (*##)
  (/) = (/##)
  _ % _ = 0.0##
  x /% y = (# x / y , 0.0## #)
instance ℤ F64 where
  negate = negateDouble#
  (//) = (/##)
  _ %% _ = 0.0##
  x //%% y = (# x / y , 0.0## #)
  abs = fabsDouble#
  sgn a = Ordering# (coerce (a > cast 0#) -# coerce (a < cast 0#))
instance ℝ F64 where
  exp = expDouble#
  expm1 = expm1Double#
  log = logDouble#
  log1p = log1pDouble#
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

instance 𝕌 U where
  log2 w = (minusWord# WORD_SIZE_IN_BITS## 1##) `minusWord#` clz w
  -- | Logarithm for an arbitrary base
  log# = \cases
   b _ | cast (b ≤ 1##) → case GHC.raiseOverflow of !_ → 0##
   2## a → log2 a
   b a → case go b of (# _, e' #) → e'
          where
            goSqr pw = case timesWord2# pw pw of
              (# 0##, l #) -> go l
              (# _  , _ #) -> (# a, 0## #)
            go pw = if cast (a < pw) then (# a, 0## #)
                    else case goSqr pw of
                     (# q, e #) -> if cast (q < pw)
                           then (# q      , 2## × e       #)
                                   else (# q % pw , 2## × e + 1## #)
instance 𝕌 U8 where
  log2 w = cast 7## `subWord8#` cast (clz w)
  log# = \cases
   b _ | cast (b ≤ cast 1##) → case GHC.raiseOverflow of !_ → cast 0##
   (cast → 2##) a → log2 a
   (cast → b) (cast → a) → case go b of (# _, e' #) → cast e'
          where
            goSqr pw = case timesWord2# pw pw of
              (# 0##, l #) -> go l
              (# _  , _ #) -> (# a, 0## #)
            go pw = if cast (a < pw) then (# a, 0## #)
                    else case goSqr pw of
                     (# q, e #) -> if cast (q < pw)
                           then (# q      , 2## × e       #)
                                   else (# q % pw , 2## × e + 1## #)
instance 𝕌 U16 where
  log2 w = cast 15## `subWord16#` cast (clz w)
  log# = \cases
   b _ | cast (b ≤ cast 1##) → case GHC.raiseOverflow of !_ → cast 0##
   (cast → 2##) a → log2 a
   (cast → b) (cast → a) → case go b of (# _, e' #) → cast e'
          where
            goSqr pw = case timesWord2# pw pw of
              (# 0##, l #) -> go l
              (# _  , _ #) -> (# a, 0## #)
            go pw = if cast (a < pw) then (# a, 0## #)
                    else case goSqr pw of
                     (# q, e #) -> if cast (q < pw)
                                   then (# q      , 2## × e       #)
                                   else (# q % pw , 2## × e + 1## #)
instance 𝕌 U32 where
  log2 w = cast 31## `subWord32#` cast (clz w)
  log# = \cases
   b _ | cast (b ≤ cast 1##) → case GHC.raiseOverflow of !_ → cast 0##
   (cast → 2##) a → log2 a
   (cast → b) (cast → a) → case go b of (# _, e' #) → cast e'
          where
            goSqr pw = case timesWord2# pw pw of
              (# 0##, l #) -> go l
              (# _  , _ #) -> (# a, 0## #)
            go pw = if cast (a < pw) then (# a, 0## #)
                    else case goSqr pw of
                     (# q, e #) -> if cast (q < pw)
                           then (# q      , 2## × e       #)
                                   else (# q % pw , 2## × e + 1## #)
instance 𝕌 U64 where
  log2 w = cast 63## `subWord64#` cast (clz w)
  log# = \cases
   b _ | cast (b ≤ cast 1##) → case GHC.raiseOverflow of !_ → cast 0##
   (cast → 2##) a → log2 a
   (cast → b) (cast → a) → case go b of (# _, e' #) → cast e'
          where
            goSqr pw = case timesWord2# pw pw of
              (# 0##, l #) -> go l
              (# _  , _ #) -> (# a, 0## #)
            go pw = if cast (a < pw) then (# a, 0## #)
                    else case goSqr pw of
                     (# q, e #) -> if cast (q < pw)
                           then (# q      , 2## × e       #)
                                   else (# q % pw , 2## × e + 1## #)
