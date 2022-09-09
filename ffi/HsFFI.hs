--------------------------------------------------------------------
-- | Description : Constants and documentation of @HsFFI.h@
--
-- See <https://gitlab.haskell.org/ghc/ghc/-/blob/master/includes/HsFFI.h HsFFI.h>
--
-- It's much more useful to
--
-- @
-- #include "HsFFI.h"
-- @
--
-- from @~/.ghcup/ghc/9.4.2/lib/ghc-9.4.2/lib/x86_64-linux-ghc-9.4.2/rts-1.0.2/include@
--
-- into a foreign c source file.
-- 
-- In haskell space, these imports are occasionally useful for sanity checks,
-- and good hooks for documentation.
--------------------------------------------------------------------
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module HsFFI (module HsFFI, module X) where
import HsFFI.Rounding as X

-- TODO figure out how to get all the constants here as macros

foreign import capi "HsFFI.h value HS_INT_MAX" int_max ∷ Int
foreign import capi "HsFFI.h value HS_INT_MIN" int_min ∷ Int
foreign import capi "HsFFI.h value HS_INT8_MAX" int8_max ∷ Int
foreign import capi "HsFFI.h value HS_INT8_MIN" int8_min ∷ Int
foreign import capi "HsFFI.h value HS_INT16_MAX" int16_max ∷ Int
foreign import capi "HsFFI.h value HS_INT16_MIN" int16_min ∷ Int
foreign import capi "HsFFI.h value HS_INT32_MAX" int32_max ∷ Int
foreign import capi "HsFFI.h value HS_INT32_MIN" int32_min ∷ Int
foreign import capi "HsFFI.h value HS_INT64_MAX" int64_max ∷ Int
foreign import capi "HsFFI.h value HS_INT64_MIN" int64_min ∷ Int

foreign import capi "HsFFI.h value HS_WORD_MAX" word_max ∷ Word
foreign import capi "HsFFI.h value HS_WORD8_MAX" word8_max ∷ Word
foreign import capi "HsFFI.h value HS_WORD16_MAX" word16_max ∷ Word
foreign import capi "HsFFI.h value HS_WORD32_MAX" word32_max ∷ Word
foreign import capi "HsFFI.h value HS_WORD64_MAX" word64_max ∷ Word

-- * 'F32'

-- | This is the value of the base, or radix, of the exponent representation.
-- This is guaranteed to be a constant expression, unlike the other macros
-- described in this section. The value is 2 on all machines we know of except
-- the IBM 360 and derivatives.
foreign import capi "HsFFI.h value HS_FLOAT_RADIX" f32_radix ∷ Int
-- | Usually 'Nearest', in accordance with IEEE standard
-- See https://www.gnu.org/software/libc/manual/html_node/Floating-Point-Parameters.html
foreign import capi "HsFFI.h value HS_FLOAT_ROUNDS" f32_rounds ∷ Rounding
-- | This is the difference between 1 and the smallest floating point number of
-- type float that is greater than 1. It’s supposed to be no greater than 1E-5.
foreign import capi "HsFFI.h value HS_FLOAT_EPSILON" f32_epsilon  ∷ Float
-- | This is the number of decimal digits of precision for the float data type.
-- Technically, if p and b are the precision and base (respectively) for the
-- representation, then the decimal precision q is the maximum number of decimal
-- digits such that any floating point number with q base 10 digits can be
-- rounded to a floating point number with p base b digits and back again,
-- without change to the q decimal digits.
--
-- The value of this macro is supposed to be at least 6, to satisfy ISO C.
foreign import capi "HsFFI.h value HS_FLOAT_DIG" f32_dig  ∷ Int
-- | This is the number of base-'f32_radix' digits in the floating point mantissa
-- for the 'F32' data type. The following expression yields 1.0 (even though
-- mathematically it should not) due to the limited number of mantissa digits:
--
-- 1.0 + 1.0 / 'f32_radix / f32_radix / … / radix
-- where radix appears @f32_mant_dig@ times.
foreign import capi "HsFFI.h value HS_FLOAT_MANT_DIG" f32_mant_dig  ∷ Int
-- | The value of this macro is the minimum normalized positive floating point number
-- that is representable in type float. It is supposed to be no more than @1E-37@.
foreign import capi "HsFFI.h value HS_FLOAT_MIN" f32_min ∷ Float

-- | This is the smallest possible exponent value for 'F32'. More
-- precisely, it is the minimum negative integer such that the value 'f32_radix'
-- raised to this power minus 1 can be represented as a normalized 'F32'.
foreign import capi "HsFFI.h value HS_FLOAT_MIN_EXP" f32_min_exp ∷ Int

-- | This is the minimum negative integer such that 10 raised to this power minus 1
-- can be represented as a normalized floating point number of type float.
-- This is supposed to be @-37@ or even less.
foreign import capi "HsFFI.h value HS_FLOAT_MIN_10_EXP" f32_min_10_exp ∷ Int

-- | The value of this macro is the maximum number representable in 'F32'.
-- It is supposed to be at least @1E+37@.
--
-- The smallest representable number is @- f32_max@.
foreign import capi "HsFFI.h value HS_FLOAT_MAX" f32_max ∷ Float

-- | This is the largest possible exponent value for type float.
-- More precisely, this is the maximum positive integer such that value 'f32_radix'
-- raised to this power minus 1 can be represented as 'F32'
foreign import capi "HsFFI.h value HS_FLOAT_MAX_EXP" f32_max_exp ∷ Int

-- | This is the maximum positive integer such that 10 raised to this power
-- minus 1 can be represented as a normalized 'F32'.
--
-- This is supposed to be at least @37@.
foreign import capi "HsFFI.h value HS_FLOAT_MAX_10_EXP" f32_max_10_exp ∷ Int

-- * 'F64'

--foreign import capi "HsFFI.h value HS_DOUBLE_RADIX" f64_radix ∷ Int
--foreign import capi "HsFFI.h value HS_DOUBLE_ROUNDS" f64_rounds ∷ Int
foreign import capi "HsFFI.h value HS_DOUBLE_EPSILON" f64_epsilon  ∷ Double
-- | This is the number of decimal digits of precision for the float data type.
-- Technically, if p and b are the precision and base (respectively) for the
-- representation, then the decimal precision q is the maximum number of decimal
-- digits such that any floating point number with q base 10 digits can be
-- rounded to a floating point number with p base b digits and back again,
-- without change to the q decimal digits.
--
-- The value of this macro is supposed to be at least 6, to satisfy ISO C.
foreign import capi "HsFFI.h value HS_DOUBLE_DIG" f64_dig  ∷ Int
-- | This is the number of base-'f64_radix' digits in the floating point mantissa
-- for the 'F64' data type. The following expression yields 1.0 (even though
-- mathematically it should not) due to the limited number of mantissa digits:
--
-- 1.0 + 1.0 / 'f64_radix / f64_radix / … / radix
-- where radix appears @f64_mant_dig@ times.
foreign import capi "HsFFI.h value HS_DOUBLE_MANT_DIG" f64_mant_dig  ∷ Int
-- | The value of this macro is the minimum normalized positive floating point number
-- that is representable in type float. It is supposed to be no more than @1E-37@.
foreign import capi "HsFFI.h value HS_DOUBLE_MIN" f64_min ∷ Double

-- | This is the smallest possible exponent value for 'F64'. More
-- precisely, it is the minimum negative integer such that the value 'f64_radix'
-- raised to this power minus 1 can be represented as a normalized 'F64'.
foreign import capi "HsFFI.h value HS_DOUBLE_MIN_EXP" f64_min_exp ∷ Int

-- | This is the minimum negative integer such that 10 raised to this power minus 1
-- can be represented as a normalized floating point number of type float.
-- This is supposed to be @-37@ or even less.
foreign import capi "HsFFI.h value HS_DOUBLE_MIN_10_EXP" f64_min_10_exp ∷ Int

-- | The value of this macro is the maximum number representable in 'F64'.
-- It is supposed to be at least @1E+37@.
--
-- The smallest representable number is @- f64_max@.
foreign import capi "HsFFI.h value HS_DOUBLE_MAX" f64_max ∷ Double

-- | This is the largest possible exponent value for type float.
-- More precisely, this is the maximum positive integer such that value 'f64_radix'
-- raised to this power minus 1 can be represented as 'F64'
foreign import capi "HsFFI.h value HS_DOUBLE_MAX_EXP" f64_max_exp ∷ Int

-- | This is the maximum positive integer such that 10 raised to this power
-- minus 1 can be represented as a normalized 'F64'.
--
-- This is supposed to be at least @37@.
foreign import capi "HsFFI.h value HS_DOUBLE_MAX_10_EXP" f64_max_10_exp ∷ Int

foreign import ccall "Rts.h performMajorGC" majorGC ∷ IO ()
foreign import ccall "Rts.h performGC" minorGC ∷ IO ()
