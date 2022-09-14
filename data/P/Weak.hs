--------------------------------------------------------------------
-- | Description : References for controlling GC aliveness and finalizers
--------------------------------------------------------------------
module P.Weak (P_Weak,Weak#
              -- * misc utilities
              ,module P.Weak
              -- * instance reexports
              ) where

new ∷ k → v → IO x → IO (P_Weak v)
new = mkWeak# 

newNoFinalizer ∷ k → v → IO (P_Weak v)
newNoFinalizer = mkWeakNoFinalizer#

-- | Add a C finalizer
addCFinalizer ∷ P# {- ^ @fptr@: C function pointer to add -} → P# {- ^ @ptr@: main argument ptr passed to @fptr(ptr)@ -} → Maybe# P# {- ^ @env@: optional environment argument passed to @fptr(env,ptr)@ -} → P_Weak v → IO B# {- ^ success -}
addCFinalizer fptr x (# B# useEnv', env #) w = coerce do addCFinalizerToWeak# fptr x useEnv' env w

-- | Retrieve the value associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
read' ∷ P_Weak v → IO (Maybe# v)
read' w s0 = case deRefWeak# w s0 of
  (# s1, alive', v #) → (# s1, (# B# alive', v #) #)

-- | Retrieve the finalizer associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
finalizer' ∷ P_Weak v → IO (Maybe# (IO x))
finalizer' w s0 = case finalizeWeak# w s0 of
  (# s1, alive', f #) → (# s1, (# B# alive', f #) #)

-- | Keep a value alive to the GC.
-- It only makes sense to apply touch to lifted types on the heap.
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch The Hidden Dangers of touch#>
touch ∷ k → IO_
touch = touch#
