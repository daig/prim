{-# language ScopedTypeVariables, NoStarIsType,UndecidableInstances #-}
--------------------------------------------------------------------
-- | Description : The promoted kind of typelevel natural numbers
--------------------------------------------------------------------
module Type.Nat (Nat,KnownNat,type (+), type (-), type (<=), type (<=?),Log2,module Type.Nat) where
--import GHC.Types as X (Nat)
import GHC.TypeNats
import GHC.Natural
import GHC.Integer
import qualified GHC.Num as GHC
import qualified GHC.Real as GHC
import Stock.Char as Stock

type family (n ∷ Nat) % (m ∷ Nat) where n % m = Mod n m
type family (n ∷ Nat) / (m ∷ Nat) where n / m = Div n m
type family (n ∷ Nat) × (m ∷ Nat) where n × m = n * m

data Proxy a = Proxy

reifyNat ∷ ∀ n. KnownNat n ⇒ Natural
reifyNat = natVal (Proxy @n)
{-# inline reifyNat #-}
reflectNat ∷ Natural → SomeNat
reflectNat n = someNatVal n
{-# inline reflectNat #-}
