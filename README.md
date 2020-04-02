# PRIM
This library reorganizes `ghc-prim` in a sane but conservative way
, without adding any fancy tricks like overloading or extra functions.
It's meant as a foundation for low-level programming in haskell, as well as (eventually),
a comprehensive home for documentation on ghc magic.

# Module Structure

  Modules live at the toplevel and do not consider the global package ecosystem, for modularity.
It is intended to use cabal mixins or PackageImports to appropriately avoid collisions downstream.
Modules are granular, intending to represent a particular usable component such as a single datatype or class.
Modules for internal types are nested under the module they group closest with,
while modules for common types like Refs and Arrays are anti-nested with the root exposing their types.

# Export Structure

  Types and constructors with the same name as the module, as well as all operators are meant to be imported unqualified.
All other identifiers are meant to be imported qualified. The common pattern is: 

```haskell
import Compact (Compact)
import Compact qualified
