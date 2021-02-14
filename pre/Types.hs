module Types (Int#,Word#,module Types,module X) where
import GHC.Types as X (RuntimeRep(..),VecCount(..),VecElem(..))
import GHC.Types (TYPE,Constraint,Bool(..))
import GHC.Prim

type B = Bool
pattern F = False
pattern T = True
{-# complete F, T #-}
pattern B ∷ I1 → B
pattern B i ← ((\i -> I1# (dataToTag# i)) → i)
  where B (I1# i) = tagToEnum# i
{-# complete B #-}

-- | The kind of constraints, like @Show a@
type C = Constraint

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T_ = TYPE ∷ RuntimeRep → T
-- | The kind of lifted types
type T = TYPE 'LiftedRep
-- | The kind of machine-int represented types
type T_I = T_ 'IntRep
type T_I8 = T_ 'Int8Rep
type T_I16 = T_ 'Int16Rep
type T_I32 = T_ 'Int32Rep
type T_I64 = T_ 'Int64Rep
-- | The kind of machine-word represented types
type T_U = T_ 'WordRep
type T_U8 = T_ 'Word8Rep
type T_U16 = T_ 'Word16Rep
type T_U32 = T_ 'Word32Rep
type T_U64 = T_ 'Word64Rep
-- | The kind of raw pointer types inhabited by 'P'
type T_P = T_ 'AddrRep
-- | The kind of unlifted array types.
type T_A = T_ 'UnliftedRep
-- | The kind of 32-bit floating point types. Inhabited by 'F32'
type T_F32 = T_ 'FloatRep
-- | The kind of 64-bit floating point types. Inhabited by 'F64'
type T_F64 = T_ 'DoubleRep

-- | 31-bit Unicode points
type Char = Char#
-- | 8-bit Latin-1 code points
newtype Char8 ∷ T_U where Char8# ∷ Char → Char8

newtype I1 ∷ T_I where I1#  ∷ Int# → I1
newtype I16  ∷ T_I where I16  ∷ Int# → I16
newtype I32  ∷ T_I where I32  ∷ Int# → I32
newtype I64  ∷ T_I where I64  ∷ Int# → I64
newtype U16  ∷ T_U where U16#  ∷ Word# → U16
newtype U32  ∷ T_U where U32#  ∷ Word# → U32
newtype U64  ∷ T_U where U64  ∷ Word# → U64



-- | An arbitrary machine address assumed to point outside the garbage-collected heap
type P# = Addr#

-- | An arbitrary machine address assumed to be a function pointer
type Fun# = P#

-- | Immutable raw pointer to a valid memory region containing some number of @x@
newtype P (x ∷ T_ r) ∷ T_P where P# ∷ ∀ r (x ∷ T_ r). P# → P x

-- | Immutable raw pointer to a valid function.
newtype Fun x ∷ T_P where Fun# ∷ ∀ x. Fun# → Fun x

type Async = TVar#
-- | A synchronising variable, used for communication between concurrent threads.
-- It can be thought of as a box, which may be empty or full.
--
-- The RTS implementation is really an abstraction for
-- connecting 'take' and 'write' calls between threads
type Sync = MVar#

-- | A mutable reference to a boxed value
type Boxed = MutVar#
type Stable = StablePtr#
{-| Pointer for controlling GC aliveness and finalizers.
 -
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
type Weak = Weak#
-- | The type constructor @Proxy#@ is used to bear witness to some
--    type variable. It\'s used when you want to pass around proxy values
--    for doing things like modelling type applications. A @Proxy#@
--    is not only unboxed, it also has a polymorphic kind, and has no
--    runtime representation, being totally free. 
type Proxy = Proxy#

-- | The Actually Uninhabited Type (unlike lifted @Void@, which contains bottom).
-- GHC cannot currently recognize it as empty, so it must be handled in case
-- matches by 'absurd', rather than the empty pattern.
newtype Void ∷ T_ ('SumRep '[]) where Void ∷ Void → Void
