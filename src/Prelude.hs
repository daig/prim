{-# OPTIONS_HADDOCK not-home  #-}
{-# language TypeOperators #-}
module Prelude (module Prelude, module X) where
import GHC.Prim as X
import           GHC.Types as X (TYPE)
import qualified GHC.Types as GHC
import ST as X (ST,ST_)
import ST.IO as X

-- TODO: use hsboot instead of this file
-- TODO: add safe versions of the sized numbers

type Char = Char#
type Char8# = Char#

type Int = Int#
type I64 = Int#
type I32 = Int#
type I16 = Int#
type I8 = Int#

type U8 = Word#
type U16 = Word#
type U32 = Word#
type U64 = Word#

type B = Int#
type B8 = Word#
type B16 = Word#
type B32 = Word#
type B64 = Word#

type F32 = Float#
type F64 = Double#

type Maybe# (a :: TYPE r) = (# B, a #)


type T = GHC.Type
type C = GHC.Constraint
