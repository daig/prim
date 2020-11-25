{-# OPTIONS_HADDOCK not-home  #-}
{-# language NoImplicitPrelude,TypeOperators #-}
module Prelude (module Prelude, module X, type TYPE) where
import GHC.Prim  as X
import Type as X
import qualified GHC.Types as GHC


-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'I.Min' and 'I.Max'.
type I = Int#

newtype I8  ∷ TYPE IntRep where I8#  ∷ I → I8
-- | Narrow a machine 'I' to 8 bits
pattern I8 ∷ I → I8
pattern I8 i ← (coerce narrow8Int# → i) where I8 = coerce
{-# complete I8 #-}

type B = GHC.Bool
type B# = I

-- | An unsigned integral type, with the same size as 'I'.
type U = Word#

newtype U8  ∷ T_U where U8#  ∷ U → U8
-- | Narrow a machine 'U' to 8 bits
pattern U8 ∷ U → U8
pattern U8 i ← (coerce narrow8Word# → i) where U8 = coerce
{-# complete U8 #-}

newtype U16 ∷ T_U where U16# ∷ U → U16
-- | Narrow a machine 'U' to 16 bits
pattern U16 ∷ U → U16
pattern U16 i ← (coerce narrow16Word# → i) where U16 = coerce
{-# complete U16 #-}

newtype U32 ∷ T_U where U32# ∷ U → U32
-- | Narrow a machine 'U' to 32 bits
pattern U32 ∷ U → U32
pattern U32 i ← (coerce narrow32Word# → i) where U32 = coerce
{-# complete U32 #-}

newtype U64 ∷ T_U where U64  ∷ U → U64

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
type F32 = Float#
-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
type F64 = Double#

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ TYPE r) = (# B# , a #)

-- | @Token@ is the primitive, unlifted type of states.
-- It has one type parameter, thus @Token (☸)@, or @Token s@,
-- where s is a type variable. The only purpose of the type parameter
-- is to keep different state threads separate. 
-- It is represented by nothing at all. 
type Token = State#
-- | A stateful computation returning an @a@, threaded by the @Token s@
type ST# s (a ∷ TYPE r) = Token s → (# Token s, a #)
-- | A stateful value action by the @Token s@
type ST_# s = Token s → Token s

-- | @☸@ is deeply magical.  It is /primitive/, but it is not
--         /unlifted/ (hence @ptrArg@).  We never manipulate values of type
--         @☸@; it\'s only used in the type system, to parameterise 'ST#'.
type (☸) = RealWorld

-- | A computation performing some I\/O before returning a value of type @a@.
type IO# (a ∷ TYPE r) = ST# (☸) a
-- | A computation performing some I\/O
type IO_# = ST_# (☸)

infix 4 >, ≥, <, ≤, ≡, ≠
class (≡) (a ∷ TYPE r) where (≡), (≠) ∷ a → a → B#
class (≤) (a ∷ TYPE r) where (>),(≥),(<),(≤) ∷ a → a → B#
