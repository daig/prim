{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}
module Types (module Types, module X) where
import GHC.Prim as X
import GHC.Types as X (TYPE,Levity(..),RuntimeRep(..),VecCount(..),VecElem(..),Constraint)

type R# (i ∷ TYPE r) = r

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T = TYPE
-- | The kind of a type
type K (a ∷ k) = k

type C = Constraint

newtype B ∷ K I where B# ∷ {unB ∷ I} ⊸ B
pattern F, T ∷ B
pattern F = B# 0#
pattern T = B# 1#


-- | 31-bit Unicode code points
type Char = Char#

-- | 8-bit Latin-1 code points
newtype Char8 ∷ K U where Char8# ∷ Char ⊸ Char8

type I = Int#
newtype I8  ∷ K I where I8#  ∷ I ⊸ I8
newtype I16 ∷ K I where I16# ∷ I ⊸ I16
newtype I32 ∷ K I where I32# ∷ I ⊸ I32
newtype I64 ∷ K I where I64  ∷ I ⊸ I64

type U = Word#
newtype U8   ∷ K U where U8#  ∷ U ⊸ U8
newtype U16  ∷ K U where U16# ∷ U ⊸ U16
newtype U32  ∷ K U where U32# ∷ U ⊸ U32
newtype U64  ∷ K U where U64  ∷ U ⊸ U64

type F32 = Float#
type F64 = Double#

type ST (s ∷ T r) (a ∷ T ra) = s ⊸ (# s , a #)
type ST_ (s ∷ T r) = s ⊸ s

-- | @(☸)@ is the primitive, unlifted type of realworld state.
-- It's only purpose is to sequence IO actions.
-- It is represented by nothing at all. 
type (☸) = State# RealWorld


-- | A computation performing some I\/O before returning a value of type @a@.
type IO (a ∷ T r)  = ST (☸) a
-- | A computation performing some I\/O
type IO_ = ST_ RealWorld

-- | The Actually Uninhabited Type (unlike lifted @X@, which contains bottom).
-- GHC cannot currently recognize it as empty, so it must be handled in case
-- matches by 'absurd', rather than the empty pattern.
newtype X ∷ T ('SumRep '[]) where X ∷ X ⊸ X

newtype A x ∷ K A# where SmallArray# ∷ SmallArray# x ⊸ A x
newtype M_A x ∷ K A# where M_SmallArray# ∷ SmallMutableArray# RealWorld x ⊸ M_A x

newtype Arr x ∷ K A# where Array# ∷ Array# x ⊸ Arr x
newtype M_Arr x ∷ K A# where M_Array# ∷ MutableArray# RealWorld x ⊸ M_Arr x

newtype A_ (x ∷ T r) ∷ K A# where A# ∷ ∀ {r} (x ∷ T r). A# ⊸ A_ x
newtype M_A_ (x ∷ T r) ∷ K A# where
  M_A# ∷ ∀ {r} (x ∷ T r). M_A# ⊸ M_A_ x

-- Unpinned
newtype A# ∷ T ('BoxedRep 'Unlifted) where UnpinnedByteArray# ∷ ByteArray# ⊸ A#
newtype M_A# ∷ K A# where M_UnpinnedByteArray# ∷ MutableByteArray# RealWorld ⊸ M_A#

newtype AA# ∷ K A# where ArrayArray# ∷ ArrayArray# ⊸ AA#
newtype M_AA# ∷ K A# where MutableArrayArray# ∷ MutableArrayArray# RealWorld ⊸ M_AA#

newtype AA (x ∷ K A#) ∷ K A# where AA# ∷ AA# ⊸ AA x
newtype M_AA (x ∷ K A#) ∷ K A# where M_AA# ∷ M_AA# ⊸ M_AA x

newtype Pinned# ∷ K A# where PinnedByteArray# ∷ ByteArray# ⊸ Pinned#
newtype M_Pinned# ∷ K A# where
  M_PinnedByteArray# ∷ MutableByteArray# RealWorld ⊸ M_Pinned#

newtype Pinned_ (x ∷ T r) ∷ K A# where
  Pinned# ∷ ∀ {r} (x ∷ T r). Pinned# ⊸ Pinned_ x
newtype M_Pinned_ (x ∷ T r) ∷ K A# where
  M_Pinned# ∷ ∀ {r} (x ∷ T r). M_Pinned# ⊸ M_Pinned_ x

type family M (a ∷ k) = (ma ∷ k) | ma → a where
  M A# = M_A#
  M (A_ (x ∷ T r)) = (M_A_ x)
  M Pinned# = M_Pinned#
  M (Pinned_ x) = (M_Pinned_ x)
  M AA# = M_AA#
  M (AA x) = M_AA x
  M (A x) = M_A x
  M (Arr x) = M_Arr x
  M (☸) = (☸)
  M P# = P#
  M (P_ (x ∷ T r)) = P_ x
  M (P x) = P x
  M (P_Async x) = P_Async x
  M (P_Sync x) = P_Sync x
  M (P_Weak x) = P_Weak x
  M (P_Stable x) = P_Stable x

-- | A C-style null-terminated string
newtype S# ∷ K P# where S# ∷ Addr# ⊸ S#

type P# = Addr#
newtype P_ (x ∷ T r) ∷ K P# where P# ∷ ∀ r (x ∷ T r). P# ⊸ P_ x

type P = MutVar# RealWorld
type P_Async = TVar# RealWorld
-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type P_Sync = MVar# RealWorld

{-|
A weak pointer expressing a relashionship between a /key/ and a /value/ of type @v@:
if the value is alive to the GC if the key is.
A reference from the value to the key does /not/ keep the key alive.
A weak pointer may also have a finalizer of type @IO_@; if it does,
then the finalizer will be run at most once, at a time after the key
has become unreachable by the program (\"dead\").  The storage manager
attempts to run the finalizer(s) for an object soon after the object
dies, but promptness is not guaranteed.
It is not guaranteed that a finalizer will eventually run, and no
attempt is made to run outstanding finalizers when the program exits.
Therefore finalizers should not be relied on to clean up resources -
other methods (eg. exception handlers) should be employed, possibly in
addition to finalizers.
References from the finalizer to the key are treated in the same way
as references from the value to the key: they do not keep the key
alive.  A finalizer may therefore ressurrect the key, perhaps by
storing it in the same data structure.
The finalizer, and the relationship between the key and the value,
exist regardless of whether the program keeps a reference to the
'Weak' object or not.
Finalizers for multiple @Weak.P@ on the same key are run in arbitrary order, or perhaps concurrently.
You must ensure there's only one finalizer if the finalizer relies on that fact.
If there are no other threads to run, the RTS will check
for runnable finalizers before declaring the system to be deadlocked.
WARNING: weak pointers to ordinary non-primitive Haskell types are
particularly fragile, because the compiler is free to optimise away or
duplicate the underlying data structure.  Therefore attempting to
place a finalizer on an ordinary Haskell type may well result in the
finalizer running earlier than you expected.  This is not a problem
for caches and memo tables where early finalization is benign.
Finalizers /can/ be used reliably for types that are created explicitly
and have identity, such as @ST.P@ and @Sync.P@.
-}
type P_Weak = Weak#
type P_Stable = StablePtr#

type Name = StableName#

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ T r)  = (# B , a #)
