module P.Stable (Stable, module X) where
import GHC.Stable as X (StablePtr(..),newStablePtr,deRefStablePtr
                       ,freeStablePtr,castStablePtrToPtr,castPtrToStablePtr) 

{- |
A /stable pointer/ is a reference to a Haskell expression that is
guaranteed not to be affected by garbage collection, i.e., it will neither be
deallocated nor will the value of the stable pointer itself change during
garbage collection (ordinary references may be relocated during garbage
collection).  Consequently, stable pointers can be passed to foreign code,
which can treat it as an opaque reference to a Haskell value.

A value of type @Stable.P a@ is a stable pointer to a Haskell
expression of type @a@.
-}
type Stable = StablePtr
