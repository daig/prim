{-# OPTIONS_HADDOCK not-home  #-}
{-# language NoImplicitPrelude,TypeOperators #-}
module Prim (module Prim, module X, type TYPE) where
import GHC.Prim (Char#,Int#,Word#,Float#,Double#,State#,RealWorld)
import           GHC.Types as X (TYPE)
import qualified GHC.Types as GHC
import Rep.RTS as X (RuntimeRep)

-- TODO: use hsboot instead of this file
-- TODO: add safe versions of the sized numbers

type Char = Char#
type Char8# = Char#

type I = Int#
type I64 = Int#
type I32 = Int#
type I16 = Int#
type I8 = Int#
type I1 = Int#

type U8 = Word#
type U16 = Word#
type U32 = Word#
type U64 = Word#


type F32 = Float#
type F64 = Double#

type Maybe# (a ∷ TYPE r) = (# I1, a #)

type T = GHC.Type
type C = GHC.Constraint

type Token = State#
type ST_ s = Token s → Token s
type ST s (a ∷ TYPE r) = Token s → (# Token s, a #)

type (☸) = RealWorld
type IO (a ∷ TYPE r) = ST (☸) a
type IO_ = ST_ (☸)
