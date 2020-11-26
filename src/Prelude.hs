{-# OPTIONS_HADDOCK not-home  #-}
{-# language NoImplicitPrelude,TypeOperators #-}
module Prelude (module Prelude, module X, type T_) where
import GHC.Prim  as X
import T as X
import qualified GHC.Types as GHC


-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'I.Min' and 'I.Max'.
type I = Int#

newtype I8  ∷ T_I where I8#  ∷ I → I8
-- | Narrow a machine 'I' to 8 bits
pattern I8 ∷ I → I8
pattern I8 i ← (coerce → i) where I8 = coerce narrow8Int#
{-# complete I8 #-}

newtype I16  ∷ T_I where I16#  ∷ I → I16
-- | Narrow a machine 'I' to 16 bits
pattern I16 ∷ I → I16
pattern I16 i ← (coerce → i) where I16 = coerce narrow16Int#
{-# complete I16 #-}

newtype I32  ∷ T_I where I32#  ∷ I → I32
-- | Narrow a machine 'I' to 32 bits
pattern I32 ∷ I → I32
pattern I32 i ← (coerce → i) where I32 = coerce narrow32Int#
{-# complete I32 #-}

newtype I64 ∷ T_I where I64  ∷ I → I64

newtype B ∷ T_I where B# ∷ I → B
pattern F ∷ B
pattern F = B# 0#
pattern T ∷ B
pattern T = B# 1#
{-# complete F, T #-}

-- | An unsigned integral type, with the same size as 'I'.
type U = Word#

newtype U8  ∷ T_U where U8#  ∷ U → U8
-- | Narrow a machine 'U' to 8 bits
pattern U8 ∷ U → U8
pattern U8 i ← (coerce → i) where U8 = coerce narrow8Word#
{-# complete U8 #-}

newtype U16 ∷ T_U where U16# ∷ U → U16
-- | Narrow a machine 'U' to 16 bits
pattern U16 ∷ U → U16
pattern U16 i ← (coerce → i) where U16 = coerce narrow16Word#
{-# complete U16 #-}

newtype U32 ∷ T_U where U32# ∷ U → U32
-- | Narrow a machine 'U' to 32 bits
pattern U32 ∷ U → U32
pattern U32 i ← (coerce → i) where U32 = coerce narrow32Word#
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
type Maybe# (a ∷ T_ r)  = (# B , a #)

-- | @Token@ is the primitive, unlifted type of states.
-- It has one type parameter, thus @Token (☸)@, or @Token s@,
-- where s is a type variable. The only purpose of the type parameter
-- is to keep different state threads separate. 
-- It is represented by nothing at all. 
type Token = State#
-- | A stateful computation returning an @a@, threaded by the @Token s@
type ST# s (a ∷ T_ r) = Token s → (# Token s, a #)
-- | A stateful value action by the @Token s@
type ST_# s = Token s → Token s

-- | @☸@ is deeply magical.  It is /primitive/, but it is not
--         /unlifted/ (hence @ptrArg@).  We never manipulate values of type
--         @☸@; it\'s only used in the type system, to parameterise 'ST#'.
type (☸) = RealWorld

-- | A computation performing some I\/O before returning a value of type @a@.
type IO# (a ∷ T_ r) = ST# (☸) a
-- | A computation performing some I\/O
type IO_# = ST_# (☸)

infix 4 >, ≥, <, ≤, ≡, ≠
class (≡) (a ∷ T_ r) where (≡), (≠) ∷ a → a → B
class (≡) a ⇒ (≤) (a ∷ T_ r) where (>),(≥),(<),(≤) ∷ a → a → B

-- | Bitwise algebriac operations
class (⊕) (a ∷ T_ r) where (∧), (∨), (⊕) ∷ a → a → a
class (¬) (a ∷ T_ r) where (¬) ∷ a → a
infixl 3 ∧
infixl 2 ⊕
infixl 1 ∨

-- | An arbitrary machine address assumed to point outside the garbage-collected heap
type P = Addr#
