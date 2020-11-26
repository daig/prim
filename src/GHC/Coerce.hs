--------------------------------------------------------------------
-- | Description : Cast between identical representations
--------------------------------------------------------------------
{-# language TypeApplications #-}
module GHC.Coerce
  (type (≑)
  ,coerce
  ,coerce#
  ,GHC.unsafeCoerce# -- | Very unsafe coerce that may not get inlined correctly.
                     -- Only use if you know it will not be called at higher order.
                     -- Magically levity polymorphic, unlike 'coerce#'
  ,GHC.Any
 ) where
import qualified Prelude as GHC
import qualified GHC.Types as GHC
import qualified Stock.Word as Stock

-- | @≑@ is a two-parameter class that has instances for types @a@ and @b@ if
--      the compiler can infer that they have the same representation. This class
--      does not have regular instances; instead they are created on-the-fly during
--      type-checking. Trying to manually declare an instance of @Coercible@
--      is an error.
--
--      Nevertheless one can pretend that the following three kinds of instances
--      exist. First, as a trivial base-case:
--
--      @instance a ≑ a@
--
--      Furthermore, for every type constructor there is
--      an instance that allows to coerce under the type constructor. For
--      example, let @D@ be a prototypical type constructor (@data@ or
--      @newtype@) with three type arguments, which have roles @nominal@,
--      @representational@ resp. @phantom@. Then there is an instance of
--      the form
--
--      @instance b ≑ b\' ⇒ D a b c ≑ D a b\' c\'@
--
--      Note that the @nominal@ type arguments are equal, the
--      @representational@ type arguments can differ, but need to have a
--      @≑@ instance themself, and the @phantom@ type arguments can be
--      changed arbitrarily.
--
--      The third kind of instance exists for every @newtype NT = MkNT T@ and
--      comes in two variants, namely
--
--      @instance a ≑ T => a ≑ NT@
--
--      @instance T ≑ b => NT ≑ b@
--
--      This instance is only usable if the constructor @MkNT@ is in scope.
--
--      If, as a library author of a type constructor like @Set a@, you
--      want to prevent a user of your module to write
--      @coerce ∷ Set T → Set NT@,
--      you need to set the role of @Set@\'s type parameter to @nominal@,
--      by writing
--
--      @type role Set nominal@
--
--      For more details about this feature, please refer to
--
-- <http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf Safe Coercions>
--      by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and Stephanie Weirich.
type (≑) = GHC.Coercible

coerce ∷ ∀ b a. a ≑ b ⇒ a → b
coerce = GHC.coerce

-- | Coerce any type into any other type.
--
--         The following uses of @unsafeCoerce\#@ are supposed to work (i.e. not lead to
--         spurious compile-time or run-time crashes (segfault)):
--
--          × Casting any lifted type to 'Any'
--
--          × Casting 'Any' back to the real type
--
--          × Casting between two types that have the same runtime representation.  One case is when
--            the two types differ only in \"phantom\" type parameters, for example
--            @Ptr Int@ to @Ptr Float@, or @[Int]@ to @[Float]@ when the list is
--            known to be empty.  Also, a @newtype@ of a type @T@ has the same representation
--            at runtime as @T@.
--
--         Other uses of @unsafeCoerce\#@ are undefined.  In particular, you should not use
--         @unsafeCoerce\#@ to cast a T to an algebraic data type D, unless T is also
--         an algebraic data type.  For example, do not cast @Int→Int@ to @Bool@, even if
--         you later cast that @Bool@ back to @Int→Int@ before applying it.  The reasons
--         have to do with GHC\'s internal representation details (for the cognoscenti, data values
--         can be entered but function closures cannot).  If you want a safe type to cast things
--         to, use @Any@, which is not an algebraic data type.
--
--
--
-- __/Warning:/__ this can fail with an unchecked exception.
-- To coerce (very unsafely) on unlifted types - use 'unsafeCoerce#' instead
coerce# ∷ ∀ b a. a → b
coerce# x = local_id (GHC.unsafeCoerce# x)
local_id ∷ a → a
local_id x = x
