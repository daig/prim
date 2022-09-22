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
addCFinalizer ∷ Addr# {- ^ @fptr@: C function pointer to add -}
              → Addr# {- ^ @ptr@: main argument ptr passed to @fptr(ptr)@ -}
              → (# (##) | Addr# #) {- ^ @env@: optional environment ptr argument passed to @fptr(env,ptr)@ -}
              → P_Weak v
              → IO B# {- ^ success -}
addCFinalizer fptr x (cast → (# B# useEnv', env #)) w = coerce do addCFinalizerToWeak# fptr x useEnv' env w

-- | Retrieve the value associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
read' ∷ ∀ v. P_Weak v → IO (# (##) | v #)
read' w = cast (coerce @_ @(IO' v) (deRefWeak# w))

-- | Retrieve the finalizer associated with a @Weak.P@ if it (the key)
-- is still alive to the GC.
finalizer' ∷ ∀ v x. P_Weak v → IO (# (##) | IO x #)
finalizer' w = cast (coerce @_ @(IO' (IO x)) (finalizeWeak# @_ @x w))
{-
finalizer' w s0 = case finalizeWeak# w s0 of
  (# s1, alive', f #) → (# s1, (# B# alive', f #) #)
  -}

-- | Keep a value alive to the GC.
-- It only makes sense to apply touch to lifted types on the heap.
--
-- see <https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch The Hidden Dangers of touch#>
touch ∷ k → IO_
touch = touch#
