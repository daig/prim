--------------------------------------------------------------------
-- | Description : Pointer for controlling GC aliveness and finalizers
--------------------------------------------------------------------
module P.Weak where


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
type P = Weak#

new ∷ k → v → IO# x → IO# (P v)
new = mkWeak# 

newNoFinalizer ∷ k → v → IO# (P v)
newNoFinalizer = mkWeakNoFinalizer#

addFinalizer ∷ P# → P# → B → P# → P v → IO# B
addFinalizer p0 p1 (B# b) p2 q = coerce do addCFinalizerToWeak# p0 p1 b p2 q

-- | Retrieve the value associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
read' ∷ P v → IO# (Maybe# v)
read' w s0 = case deRefWeak# w s0 of
  (# s1, alive', v #) → (# s1, (# B# alive', v #) #)

-- | Retrieve the finalizer associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
finalizer' ∷ P v → IO# (Maybe# (IO# x))
finalizer' w s0 = case finalizeWeak# w s0 of
  (# s1, alive', f #) → (# s1, (# B# alive', f #) #)

-- | Keep a value alive to the GC.
-- It only makes sense to apply touch to lifted types on the heap.
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch The Hidden Dangers of touch#>
touch ∷ k → IO_#
touch = touch#
