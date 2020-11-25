{-# language NoImplicitPrelude #-}
module Type (module Type, module X) where
import GHC.Types as X (TYPE,RuntimeRep(..),VecCount(..),VecElem(..))
import qualified GHC.Types as GHC

-- | The kind of constraints, like @Show a@
type C = GHC.Constraint

type T_ = TYPE
type T = TYPE 'LiftedRep
type T_I = TYPE 'IntRep
type T_I8 = TYPE 'Int8Rep
type T_I16 = TYPE 'Int16Rep
type T_I32 = TYPE 'Int32Rep
type T_I64 = TYPE 'Int64Rep
type T_U = TYPE 'WordRep
type T_U8 = TYPE 'Word8Rep
type T_U16 = TYPE 'Word16Rep
type T_U32 = TYPE 'Word32Rep
type T_U64 = TYPE 'Word64Rep
type T_P = TYPE 'AddrRep
type T_A = TYPE 'UnliftedRep
type T_F32 = TYPE 'FloatRep
type T_F64 = TYPE 'DoubleRep
