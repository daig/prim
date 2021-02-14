module P (P, module X) where
import GHC.Ptr as X (Ptr(..),nullPtr,castPtr,plusPtr,alignPtr,minusPtr)
import I as X (I)

newtype P a = Ptr (Ptr a)
-- ^ A value of type @P a@ represents a pointer to an object, or an
-- array of objects, which may be marshalled to or from Haskell values
-- of type @a@.
--
-- The type @a@ will often be an instance of class
-- 'Foreign.Storable.Storable' which provides the marshalling operations.
-- However this is not essential, and you can provide your own operations
-- to access the pointer.  For example you might write small foreign
-- functions to get or set the fields of a C @struct@.

-- |The constant 'nullPtr' contains a distinguished value of 'P'
-- that is not associated with a valid memory location.

null ∷ P a
null = coerce nullPtr

(∔) ∷ P a → I → P a
(∔) = coerce plusPtr

(߸) ∷ P a → P b → I
(߸) = coerce minusPtr


-- |Given an arbitrary address and an alignment constraint,
-- 'align' yields the next higher address that fulfills the
-- alignment constraint.  An alignment constraint @x@ is fulfilled by
-- any address divisible by @x@.  This operation is idempotent.
align ∷ P a → I → P a
align = coerce alignPtr
