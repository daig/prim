{-# language PackageImports #-}
{-# language NoImplicitPrelude #-}
module Prim (module Prim, module X) where
import Type as X (T,T_,RuntimeRep(..))
import qualified Prim.B as Prim
import qualified Prim.I as Prim
import qualified Prim.I8 as Prim
import qualified Prim.I16 as Prim
import qualified Prim.I32 as Prim
import qualified Prim.I64 as Prim
import qualified Prim.U as Prim
import qualified Prim.U8 as Prim
import qualified Prim.U16 as Prim
import qualified Prim.U32 as Prim
import qualified Prim.U64 as Prim
import qualified Prim.F32 as Prim
import qualified Prim.F64 as Prim
import qualified Prim.P as Prim
import qualified Prim.P.Stable as Prim

import B
import I
import I8
import I16
import I32
import I64
import U
import U8
import U16
import U32
import U64
import F32
import F64
import P
import P.Fun as P
import P.Stable as P

type family ΞRep (a ∷ T) ∷ RuntimeRep where
  ΞRep B = IntRep
  ΞRep I = IntRep
  ΞRep I8 = IntRep
  ΞRep I16 = IntRep
  ΞRep I32 = IntRep
  ΞRep I64 = IntRep
  ΞRep U = WordRep
  ΞRep U8 = WordRep
  ΞRep U16 = WordRep
  ΞRep U32 = WordRep
  ΞRep U64 = WordRep
  ΞRep F32 = FloatRep
  ΞRep F64 = DoubleRep
  ΞRep (P (a ∷ T)) = AddrRep
  ΞRep (Fun a) = AddrRep
  ΞRep (Stable a) = AddrRep
type family Ξ (a ∷ T) = (b ∷ T_ (ΞRep a)) | b → a where
  Ξ B = Prim.B
  Ξ I = Prim.I
  Ξ I8 = Prim.I8
  Ξ I16 = Prim.I16
  Ξ I32 = Prim.I32
  Ξ I64 = Prim.I64
  Ξ U = Prim.U
  Ξ U8 = Prim.U8
  Ξ U16 = Prim.U16
  Ξ U32 = Prim.U32
  Ξ U64 = Prim.U64
  Ξ F32 = Prim.F32
  Ξ F64 = Prim.F64
  Ξ (P a) = Prim.P a
  Ξ (Fun a) = Prim.Fun a
  Ξ (Stable a) = Prim.Stable a
