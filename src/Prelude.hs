{-# OPTIONS_HADDOCK not-home  #-}
{-# language NoImplicitPrelude,TypeOperators #-}
module Prelude (module Prelude, module X, type TYPE) where
import GHC.Prim  as X
import GHC.Types as X (TYPE,RuntimeRep(..),VecCount(..),VecElem(..))
import qualified GHC.Types as GHC


-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'I.Min' and 'I.Max'.
type I = Int#
newtype I64 ∷ TYPE IntRep where I64  ∷ I → I64
newtype I32 ∷ TYPE IntRep where I32# ∷ I → I32
newtype I16 ∷ TYPE IntRep where I16# ∷ I → I16
newtype I8  ∷ TYPE IntRep where I8#  ∷ I → I8

type B = GHC.Bool
type B# = I

-- | An unsigned integral type, with the same size as 'I'.
type U = Word#
newtype U64 ∷ TYPE WordRep where U64  ∷ U → U64
newtype U32 ∷ TYPE WordRep where U32# ∷ U → U32
newtype U16 ∷ TYPE WordRep where U16# ∷ U → U16
newtype U8  ∷ TYPE WordRep where U8#  ∷ U → U8

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

-- | The kind of types with lifted values. For example @Int :: Type@.
type T = TYPE 'LiftedRep
-- | The kind of constraints, like @Show a@
type C = GHC.Constraint

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

class (≡) (a ∷ TYPE r) where (≡) ∷ a → a → B#
