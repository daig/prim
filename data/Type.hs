--------------------------------------------------------------------
-- | Description : Kinds of Types and Constraints
--------------------------------------------------------------------
{-# language NoImplicitPrelude #-}
module Type (module Type, module X) where
import GHC.Types as X (TYPE,RuntimeRep(..),VecCount(..),VecElem(..))

-- | The kind constructor of types abstracted over 'RuntimeRep'
type T_ = TYPE
-- | The kind of lifted types
type T = TYPE 'LiftedRep
-- | The kind of machine-int represented types
type T_I = T_ 'IntRep
type T_I8 = T_ 'Int8Rep
type T_I16 = T_ 'Int16Rep
type T_I32 = T_ 'Int32Rep
type T_I64 = T_ 'Int64Rep
-- | The kind of machine-word represented types
type T_U = T_ 'WordRep
type T_U8 = T_ 'Word8Rep
type T_U16 = T_ 'Word16Rep
type T_U32 = T_ 'Word32Rep
type T_U64 = T_ 'Word64Rep
-- | The kind of raw pointer types inhabited by 'P'
type T_P = T_ 'AddrRep
-- | The kind of unlifted array types.
type T_A = T_ 'UnliftedRep
-- | The kind of 32-bit floating point types. Inhabited by 'F32'
type T_F32 = T_ 'FloatRep
-- | The kind of 64-bit floating point types. Inhabited by 'F64'
type T_F64 = T_ 'DoubleRep
