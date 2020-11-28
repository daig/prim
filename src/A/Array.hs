-- | Description : /Unlifted/ arrays. Containing strictly evaluated pointers.
-- Unlike "A.Byte", these store pointers, but like it, they're guarenteed not to be bottom.
-- Eg. We cannot store a 'P', because it might be null, but we can store "A.Byte",
-- and more "A.Array"
module A.Array where
import A
import qualified A.Byte as Byte
import qualified P.Stable.Name as Name
import I
import IO (run)

type A = ArrayArray#
type MA = MutableArrayArray#
-- | "Refs"
type instance M A s = MA s
type instance M (Name.P a) s = Name.P a

instance (≡) (MA s) where
  (≡) = coerce sameMutableArrayArray#
  as ≠ bs = (¬) (as ≡ bs)

-- | @thaw##@ is just a cast
--   @new@ size in elements. Uninitialized entries point recursively to the array itself.
--   @lenM#@ is safe
instance 𝔸 A where
  new# = newArrayArray#
  freeze## = unsafeFreezeArrayArray#
  freeze# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → freeze## ma s''
  thaw## a s = (# s , unsafeCoerce# a #)
  thaw# a off n s = case new# n s of
    (# s' , ma #) → case copy a off ma 0# n s' of s'' → (# s'' , ma #)
  len = sizeofArrayArray#
  lenM# = sizeofMutableArrayArray#
  lenM ma = \s → (# s , sizeofMutableArrayArray# ma #)
  clone# a off n = run \ s → case thaw## a s of
    (# s' , ma #) → case freeze# ma off n s' of (# _ , a' #) → a'
  cloneM# ma off n = \s → case new# n s of
    (# s' , ma' #) → case copy ma off ma 0# n s' of s'' → (# s'' , ma' #)

instance Copy A (MA s) s where copy = copyArrayArray#
instance Copy (MA s) (MA s) s where copy = copyMutableArrayArray#

instance (♯) a ⇒ a ∈ A where
  new n x = \s0 → case new# @A n s0 of
    (# s1 , ma #) → let go = \case {0# → \s → s; i → \s → case write# ma i x s of s' → go (n - 1#) s'}
                    in case go (n - 1# ) s1 of s2 → (# s2, ma #)
  index# = indexAA#
  read# = readAA#
  write# = writeAA#
  
class (♯) (a ∷ T_A) where
  indexAA# ∷ A → I → a
  readAA# ∷ MA s → I → ST# s a
  writeAA# ∷ MA s → I → a → ST_# s
  readM# ∷ MA s → I → ST# s (M a s)
  writeM# ∷ MA s → I → M a s → ST_# s
instance (♯) Byte.A where
  indexAA# = indexByteArrayArray#
  readAA# = readByteArrayArray#
  writeAA# = writeByteArrayArray#
  readM# = readMutableByteArrayArray#
  writeM# = writeMutableByteArrayArray#
instance (♯) A where
  indexAA# = indexArrayArrayArray#
  readAA# = readArrayArrayArray#
  writeAA# = writeArrayArrayArray#
  readM# = readMutableArrayArrayArray#
  writeM# = writeMutableArrayArrayArray#
-- | /Warning/: unsafe for FFI use.
instance (♯) (Name.P a) where
  indexAA# = unsafeCoerce# indexArrayArrayArray#
  readAA# = unsafeCoerce# readArrayArrayArray#
  writeAA# = unsafeCoerce# writeArrayArrayArray#
  readM# = unsafeCoerce# readArrayArrayArray#
  writeM# = unsafeCoerce# writeArrayArrayArray#
