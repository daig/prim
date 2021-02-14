{-# OPTIONS_HADDOCK ignore-exports #-}
{-# language LinearTypes #-}
module Num where
import Cmp
import Bits
import Cast
import HsFFI
import GHC.Word (Word(..))
import GHC.Int (Int(..))

-- |Satisfies @((((x / y) × y) + (x % y) ≡ x@. The
class (≤) a ⇒ ℕ (a ∷ T r) where
  (+), (×) ∷ a ⊸ a ⊸ a
  -- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
  (/), (%) ∷ a {- ^ dividend -}  ⊸ a {- ^ divisor -} ⊸ a
  -- | Satisfies @((x / y) + ((x % y) × y) ≡ x@.
  (/%) ∷ a ⊸ a ⊸ (# a , a #)
  -- |Add reporting overflow.
  addC ∷ a ⊸ a ⊸ (# a, B #) -- ^ The truncated sum and whether it overflowed
  -- |Subtract reporting overflow
  subC ∷ a ⊸ a ⊸ (# a, B #) -- ^ The truncated subtraction and whether it underflowed
  min ∷ (##) ⊸ a
  max ∷ (##) ⊸ a
class ℕ a ⇒ ℤ (a ∷ T r) where
  -- |Satisfies @((((x // y) × y) + (x %% y) ≡ x@.
  (//),(%%) ∷ a ⊸ a ⊸ a
  -- | Rounds towards 0. The behavior is undefined if the first argument is zero.
  (//%%) ∷ a ⊸ a ⊸ (# a , a #)
  (-) ∷ a ⊸ a ⊸ a
  negate ∷ a ⊸ a
class ℤ a ⇒ ℝ (a ∷ T r) where
  abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ a ⊸ a
  (**) ∷ a ⊸ a ⊸ a

instance ℕ U where
  (+) = λ\i → λ do plusWord# i
  (×) = λ\i → λ do timesWord# i
  (/) = λ\i → λ do quotWord# i
  (%) = λ\i → λ do remWord# i
  (/%) = λ\i → λ do quotRemWord# i
  addC a b  = go do (λ\x → λ\y → addWordC# x y) a b where
    go ∷ (# U , I #) ⊸ (# U , B #)
    go (# x, p #) = (# x , p ≠ 0# #)
  subC a b = go do (λ\x → λ\y → subWordC# x y) a b where
    go ∷ (# U , I #) ⊸ (# U , B #)
    go (# x, p #) = (# x , p ≠ 0# #)

instance ℕ U8 where
  U8# x + U8# y = cast do x + y
  U8# x × U8# y = cast do x × y
  U8# x / U8# y = cast do x / y
  U8# x % U8# y = cast do x % y
  U8# x /% U8# y = λ coerce do x /% y
  addC = λ\(U8# a) → λ\(U8# b) → let c = a + b
                                        in (# cast c , U8# c > max (##) #)
  subC (U8# a) (U8# b) = go do subC a b where
    go ∷ (# U , B #) ⊸ (# U8, B #)
    go (# x, b #) = (# cast x , b #)
  max (##) = case word8_max of W# w → U8#  w
  min (##) = U8# 0##

instance ℕ U16 where
  U16# x + U16# y = cast do x + y
  U16# x × U16# y = cast do x × y
  U16# x / U16# y = cast do x / y
  U16# x % U16# y = cast do x % y
  U16# x /% U16# y = λ coerce do x /% y
  addC = λ\(U16# a) → λ\(U16# b) → let c = a + b
                                        in (# cast c , U16# c > max (##) #)
  subC (U16# a) (U16# b) = go do subC a b where
    go ∷ (# U , B #) ⊸ (# U16, B #)
    go (# x, b #) = (# cast x , b #)
  max (##) = case word16_max of W# w → U16# w
  min (##) = U16# 0##

instance ℕ U32 where
  U32# x + U32# y = cast do x + y
  U32# x × U32# y = cast do x × y
  U32# x / U32# y = cast do x / y
  U32# x % U32# y = cast do x % y
  U32# x /% U32# y = λ coerce do x /% y
  addC = λ\(U32# a) → λ\(U32# b) → let c = a + b
                                        in (# cast c , U32# c > max (##) #)
  subC (U32# a) (U32# b) = go do subC a b where
    go ∷ (# U , B #) ⊸ (# U32, B #)
    go (# x, b #) = (# cast x , b #)
  max (##) = case word32_max of W# w → U32# w
  min (##) = U32# 0##

instance ℕ U64 where
  U64 x + U64 y = cast do x + y
  U64 x × U64 y = cast do x × y
  U64 x / U64 y = cast do x / y
  U64 x % U64 y = cast do x % y
  U64 x /% U64 y = λ coerce do x /% y
  addC = λ\(U64 a) → λ\(U64 b) → let c = a + b
                                      in (# cast c , U64 c > max (##) #)
  subC (U64 a) (U64 b) = go do subC a b where
    go ∷ (# U , B #) ⊸ (# U64, B #)
    go (# x, b #) = (# cast x , b #)
  max (##) = case word64_max of W# w → U64 w
  min (##) = U64 0##

-- | Low word of signed integer multiply
-- 
-- Modular functions have built-in rules.
instance ℕ I where
  (+) = λ\a → λ (a +#)
  (×) = λ\a → λ (a *#)
  (%) = λ\i → λ do modInt# i
  (/) = λ\i → λ do divInt# i
  (/%) = go  where
    go = λ\x → λ\y →  case 0# < x ∧ 0# > y of
      T → case (x - 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
      F → case 0# > x ∧ 0# < y of
        T → case (x + 1# ) //%% y of (# q, r #) → (# q - 1#, r + y + 1# #)
        F → x //%% y
  addC = coerce (λ\i → λ do addIntC# i)
  subC = coerce (λ\i → λ do subIntC# i)
instance ℤ I where
  negate = λ negateInt#
  (-) = λ\a → λ (a -#)
  (//) = λ\i → λ do quotInt# i
  (%%) = λ\i → λ do remInt# i
  (//%%) = λ\i → λ do quotRemInt# i

instance ℕ I8 where
  I8# x +  I8# y = cast (x + y)
  I8# x ×  I8# y = cast (x × y)
  I8# x /  I8# y = cast (x / y)
  I8# x %  I8# y = cast (x % y)
  I8# x /% I8# y = λ coerce do x /% y
  addC = λ\(I8# a) → λ\(I8# b) → let c = a + b in (# cast c , I8# c > max (##) #)
  subC = λ\(I8# a) → λ\(I8# b) → let c = a - b in (# cast c , I8# c < min (##) #)
  max (##) = case int8_max of I# i → I8# i
  min (##) = case int8_min of I# i → I8# i
instance ℤ I8 where
  negate (I8# x) = cast (negate x)
  (I8# x) - (I8# y) = cast (x - y)
  I8# x // I8# y = cast (x // y)
  I8# x %% I8# y = cast (x %% y)
  I8# x //%% I8# y = go (x //%% y) where
    go ∷ (# I , I #) ⊸ (# I8 , I8 #)
    go (# q, r #) = (# cast q, cast r #)

instance ℕ I16 where
  I16# x +  I16# y = cast (x + y)
  I16# x ×  I16# y = cast (x × y)
  I16# x /  I16# y = cast (x / y)
  I16# x %  I16# y = cast (x % y)
  I16# x /% I16# y = λ coerce do x /% y
  addC = λ\(I16# a) → λ\(I16# b) → let c = a + b in (# cast c , I16# c > max (##) #)
  subC = λ\(I16# a) → λ\(I16# b) → let c = a - b in (# cast c , I16# c < min (##) #)
  max (##) = case int16_max of I# i → I16# i
  min (##) = case int16_min of I# i → I16# i
instance ℤ I16 where
  negate (I16# x) = cast (negate x)
  (I16# x) - (I16# y) = cast (x - y)
  I16# x // I16# y = cast (x // y)
  I16# x %% I16# y = cast (x %% y)
  I16# x //%% I16# y = go (x //%% y) where
    go ∷ (# I , I #) ⊸ (# I16 , I16 #)
    go (# q, r #) = (# cast q, cast r #)

instance ℕ I32 where
  I32# x +  I32# y = cast (x + y)
  I32# x ×  I32# y = cast (x × y)
  I32# x /  I32# y = cast (x / y)
  I32# x %  I32# y = cast (x % y)
  I32# x /% I32# y = λ coerce do x /% y
  addC = λ\(I32# a) → λ\(I32# b) → let c = a + b in (# cast c , I32# c > max (##) #)
  subC = λ\(I32# a) → λ\(I32# b) → let c = a - b in (# cast c , I32# c < min (##) #)
  max (##) = case int32_max of I# i → I32# i
  min (##) = case int32_min of I# i → I32# i
instance ℤ I32 where
  negate (I32# x) = cast (negate x)
  (I32# x) - (I32# y) = cast (x - y)
  I32# x // I32# y = cast (x // y)
  I32# x %% I32# y = cast (x %% y)
  I32# x //%% I32# y = go (x //%% y) where
    go ∷ (# I , I #) ⊸ (# I32 , I32 #)
    go (# q, r #) = (# cast q, cast r #)

instance ℕ I64 where
  I64 x +  I64 y = cast (x + y)
  I64 x ×  I64 y = cast (x × y)
  I64 x /  I64 y = cast (x / y)
  I64 x %  I64 y = cast (x % y)
  I64 x /% I64 y = λ coerce do x /% y
  addC = λ\(I64 a) → λ\(I64 b) → let c = a + b in (# cast c , I64 c > max (##) #)
  subC = λ\(I64 a) → λ\(I64 b) → let c = a - b in (# cast c , I64 c < min (##) #)
  max (##) = case int64_max of I# i → I64 i
  min (##) = case int64_min of I# i → I64 i
instance ℤ I64 where
  negate (I64 x) = cast (negate x)
  (I64 x) - (I64 y) = cast (x - y)
  I64 x // I64 y = cast (x // y)
  I64 x %% I64 y = cast (x %% y)
  I64 x //%% I64 y = go (x //%% y) where
    go ∷ (# I , I #) ⊸ (# I64 , I64 #)
    go (# q, r #) = (# cast q, cast r #)

instance ℕ F32 where
  (+) = λ\i → λ do plusFloat# i
  (×) = λ\i → λ do timesFloat# i
  (/) = λ\i → λ do divideFloat# i
  (%)= λ\_ → λ\_ → 0.0#
  x /% y = (# x / y , 0.0# #)
  addC = λ\a → λ\b → let c = a + b in (# c , c ≡ (1.0# / 0.0# ) #)
  subC = λ\a → λ\b → let c = a - b in (# c , c ≡ (-1.0# / 0.0# ) #)
instance ℤ F32 where
  negate = λ negateFloat#
  (-) = λ\i → λ do minusFloat# i
  (//) = λ\i → λ do divideFloat# i
  (%%) = λ\_ → λ\_ → 0.0#
  x //%% y = (# x / y , 0.0# #)
instance ℝ F32 where
  abs = λ fabsFloat#
  exp = λ expFloat#
  log = λ logFloat#
  sqrt = λ sqrtFloat#
  sin = λ sinFloat#
  cos = λ cosFloat#
  tan = λ tanFloat#
  asin = λ asinFloat#
  acos = λ acosFloat#
  atan = λ atanFloat#
  sinh = λ sinhFloat#
  cosh = λ coshFloat#
  tanh = λ tanhFloat#
  (**) = λ\i → λ do powerFloat# i
instance ℕ F64 where
  (+) = λ\a → λ (a +##)
  (×) = λ\a → λ (a *##)
  (/) = λ\a → λ (a /##)
  (%)= λ\_ → λ\_ → 0.0##
  x /% y = (# x / y , 0.0## #)
  addC = λ\a → λ\b → let c = a + b in (# c , c ≡ (1.0## / 0.0## ) #)
  subC = λ\a → λ\b → let c = a - b in (# c , c ≡ (-1.0## / 0.0## ) #)
instance ℤ F64 where
  negate = λ negateDouble#
  (-) = λ\a → λ (a -##)
  (//) = λ\a → λ (a /##)
  (%%) = λ\_ → λ\_ → 0.0##
  x //%% y = (# x / y , 0.0## #)
instance ℝ F64 where
  abs = λ fabsDouble#
  exp = λ expDouble#
  log = λ logDouble#
  sqrt = λ sqrtDouble#
  sin = λ sinDouble#
  cos = λ cosDouble#
  tan = λ tanDouble#
  asin = λ asinDouble#
  acos = λ acosDouble#
  atan = λ atanDouble#
  sinh = λ sinhDouble#
  cosh = λ coshDouble#
  tanh = λ tanhDouble#
  (**) = λ\a → λ (a **##)
