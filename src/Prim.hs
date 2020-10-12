{-# OPTIONS_HADDOCK not-home  #-}
{-# language NoImplicitPrelude,TypeOperators #-}
module Prim (module Prim, module X, type TYPE) where
import GHC.Prim (Char#,Int#,Int16#,Int8#,Word#,Word16#,Word8#,Float#,Double#,State#,RealWorld)
import           GHC.Types as X (TYPE)
import qualified GHC.Types as GHC
import Rep.RTS as X (RuntimeRep)

-- TODO: use hsboot instead of this file
-- TODO: add safe versions of the sized numbers

type Char = Char#
type Char8# = Char#

type I = Int#
type I64 = I
type I32# = I
type I16# = I
type I8# = I
type B# = I
type I16 = Int16#
type I8 = Int8#

type U = Word#
type U64 = U
type U32# = U
type U16# = U
type U8# = U
type U16 = Word16#
type U8 = Word8#


type F32 = Float#
type F64 = Double#

type Maybe# (a ∷ TYPE r) = (# B# , a #)

type T = GHC.Type
type C = GHC.Constraint

type Token = State#
type ST_ s = Token s → Token s
type ST s (a ∷ TYPE r) = Token s → (# Token s, a #)

type (☸) = RealWorld
type IO (a ∷ TYPE r) = ST (☸) a
type IO_ = ST_ (☸)
