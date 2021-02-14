--------------------------------------------------------------------
-- | Description : The promoted kind of typelevel strings
--------------------------------------------------------------------
{-# language ScopedTypeVariables, UndecidableInstances #-}
module Type.Symbol (type Symbol) where
--import GHC.Types as X (Nat)
import GHC.TypeLits
import GHC.Natural
import GHC.Integer
import qualified GHC.Num as GHC
import qualified GHC.Real as GHC
import Stock.Char as Stock
import Data.Ord as Stock

type family (n ∷ Symbol) ++ (m ∷ Symbol) where n ++ m = AppendSymbol n m

data Proxy a = Proxy

type family Cmp (a ∷ k) (b ∷ k) ∷ Stock.Ordering where
  Cmp (a ∷ Symbol) (b ∷ Symbol) = CmpSymbol a b
  Cmp (a ∷ Nat) (b ∷ Nat) = CmpNat a b

reifySymbol ∷ ∀ s. KnownSymbol s ⇒ [Stock.Char]
reifySymbol = symbolVal (Proxy @s)
{-# inline reifySymbol #-}
reflectSymbol ∷ [Stock.Char] → SomeSymbol
reflectSymbol n = someSymbolVal n
{-# inline reflectSymbol #-}
