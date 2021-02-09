-- | Description : /Unlifted/ arrays. Containing strictly evaluated pointers.
-- Unlike "A.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store a 'P', because it might be null, but we can store "A.Byte",
-- and more "A.Array"
module Prim.A.Array where
import Prim.A
import qualified Prim.A.Prim as Prim
import qualified P.Stable.Name as Name
import qualified P.STM as STM
import qualified P.Boxed as Boxed
import qualified P.Weak as Weak
import qualified P.Sync as Sync
import Prim.I
import Prim.IO (run)
import GHC.Coerce


newtype A (x ∷ T_A) ∷ T_A where A# ∷ Refs → A x
newtype MA s (x ∷ T_A) ∷ T_A where MA# ∷ MRefs s → MA s x
-- | "A.Array"
-- TODO janky
--type instance M (Name.P a) s = Name.P a
--type instance M (Sync.P a) s = Sync.P a 
--type instance M (Weak.P a) s = Weak.P a
--type instance M (Boxed.P a) s = Boxed.P a
--type instance M (STM.P a) s = STM.P a

instance (≡) (MA s x) where
  (≡) = coerce sameMutableArrayArray#
  as ≠ bs = (¬) (as ≡ bs)

-- | "A.Array" -
-- @thaw##@ is just a cast.
-- @new#@ size in elements. Uninitialized entries point recursively to the array itself.
-- @lenM#@ is safe.
instance 𝔸 (A x) where
  new# = coerce newArrayArray#
  freeze## = coerce unsafeFreezeArrayArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  len = coerce sizeofArrayArray#
  lenM# = coerce sizeofMutableArrayArray#
  lenM (MA# ma) = \s → (# s , sizeofMutableArrayArray# ma #)
  clone# a off n = run \ s → case thaw## a s of
    (# s' , ma #) → case freeze# ma off n s' of (# _ , a' #) → a'
  cloneM# ma off n = \s → case new# n s of
    (# s' , ma' #) → case copy ma off ma 0# n s' of s'' → (# s'' , ma' #)

instance Copy (A a) (MA s a) s where copy = coerce copyArrayArray#
instance Copy (MA s a) (MA s a) s where copy = coerce copyMutableArrayArray#

instance (♯) a ⇒ a ∈ (A a) where
  new n x = \s0 → case new# @(A _) n s0 of
    (# s1 , ma #) → let go = \case {0# → \s → s; i → \s → case write# ma i x s of s' → go (n - 1#) s'}
                    in case go (n - 1# ) s1 of s2 → (# s2, ma #)
  index# (A# a) = indexAA# a
  read# (MA# ma) = readAA# ma
  write# (MA# ma) = writeAA# ma
  set ma x = go (lenM# ma - 1#) where
    go 0# s = write# ma 0# x s
    go i s = go (i - 1#) (write# ma i x s)

-- | Primitive boxed unlifted types that fit natively into Unlifted Arrays
class (♯) (a ∷ T_A) where
  indexAA# ∷ Refs → I → a
  readAA# ∷ MRefs s → I → ST# s a
  writeAA# ∷ MRefs s → I → a → ST_# s
  readM# ∷ MRefs s → I → ST# s (M a s)
  writeM# ∷ MRefs s → I → M a s → ST_# s
instance (♯) Bytes where
  indexAA# = coerce indexByteArrayArray#
  readAA# = coerce readByteArrayArray#
  writeAA# = coerce writeByteArrayArray#
  readM# = coerce readMutableByteArrayArray#
  writeM# = coerce writeMutableByteArrayArray#
-- | "A.Prim"
instance (♯) (Prim.A x) where
  indexAA# = coerce indexByteArrayArray#
  readAA# = coerce readByteArrayArray#
  writeAA# = coerce writeByteArrayArray#
  readM# = coerce readMutableByteArrayArray#
  writeM# = coerce writeMutableByteArrayArray#
instance (♯) Refs where
  indexAA# = indexArrayArrayArray#
  readAA# = readArrayArrayArray#
  writeAA# = writeArrayArrayArray#
  readM# = readMutableArrayArrayArray#
  writeM# = writeMutableArrayArrayArray#
instance (♯) (A x) where
  indexAA# = coerce indexArrayArrayArray#
  readAA# = coerce readArrayArrayArray#
  writeAA# = coerce writeArrayArrayArray#
  readM# = coerce readMutableArrayArrayArray#
  writeM# = coerce writeMutableArrayArrayArray#
-- | "P.Stable.Name" /Warning/: unsafe for FFI use.
instance (♯) (Name.P a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
-- | "P.STM" /Warning/: unsafe for FFI use.
instance (♯) (STM.P s a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
-- | "P.Boxed" /Warning/: unsafe for FFI use.
instance (♯) (Boxed.P s a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
-- | "P.Weak" /Warning/: unsafe for FFI use.
instance (♯) (Weak.P a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
-- | "P.Sync" /Warning/: unsafe for FFI use.
instance (♯) (Sync.P s a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
