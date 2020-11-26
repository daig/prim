--------------------------------------------------------------------
-- | Description : Kinds of Types and Constraints
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module T (module T, module X) where
import GHC.Types as X (TYPE,RuntimeRep(..),VecCount(..),VecElem(..))
import qualified GHC.Types as GHC

-- | The kind of constraints, like @Show a@
type C = GHC.Constraint

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T_ = TYPE
-- | The kind of lifted types
type T = TYPE 'LiftedRep
-- | The kind of machine-int represented types
type T_I = TYPE 'IntRep
type T_I8 = TYPE 'Int8Rep
type T_I16 = TYPE 'Int16Rep
type T_I32 = TYPE 'Int32Rep
type T_I64 = TYPE 'Int64Rep
-- | The kind of machine-word represented types
type T_U = TYPE 'WordRep
type T_U8 = TYPE 'Word8Rep
type T_U16 = TYPE 'Word16Rep
type T_U32 = TYPE 'Word32Rep
type T_U64 = TYPE 'Word64Rep
-- | The kind of raw pointer types inhabited by 'P'
type T_P = TYPE 'AddrRep
-- | The kind of unlifted array types.
type T_A = TYPE 'UnliftedRep
-- | The kind of 32-bit floating point types. Inhabited by 'F32'
type T_F32 = TYPE 'FloatRep
-- | The kind of 64-bit floating point types. Inhabited by 'F64'
type T_F64 = TYPE 'DoubleRep
