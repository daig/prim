-- | Types for arrays, meant to be imported qualified
{-# language NoImplicitPrelude #-}
module Array where
import GHC.Prim

type Boxed = Array#
type Unlifted = ArrayArray#
type Small = SmallArray#
type Byte = ByteArray#

type I64 = ByteArray#
type I32 = ByteArray#
type I16 = ByteArray#
type I8 = ByteArray#

type U64 = ByteArray#
type U32 = ByteArray#
type U16 = ByteArray#
type U8 = ByteArray#

type F32 = ByteArray#
type F64 = ByteArray#

type StablePtr = ByteArray#
