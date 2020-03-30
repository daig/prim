module Prelude (module Prelude, module X) where
import GHC.Prim as X

type Char = Char#

type Int = Int#
type I8 = Int#
type I16 = Int#
type I32 = Int#
type I64 = Int#

type Word = Word#
type U8 = Word#
type U16 = Word#
type U32 = Word#
type U64 = Word#

newtype B = B# Int#
type B8 = Word#
type B16 = Word#
type B32 = Word#
type B64 = Word#

type F32 = Float#
type F64 = Double#
