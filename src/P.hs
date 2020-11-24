{-# language TypeSynonymInstances,UnliftedNewtypes, GADTs, TypeOperators #-}
module P where
import P.Byte
import qualified P.I64 as I64

class Prim (a ∷ TYPE r) where
  index# ∷ P → I → a
  read# ∷ P → I → ST# s a
  write# ∷ P → I → a → ST_# s
{-
 -instance Prim I64 where
 -  index# = indexInt64OffAddr#
 -  read# = readInt64OffAddr#
 -  write# = writeInt64OffAddr#
 -instance Prim I32# where
 -  index# = indexInt32OffAddr#
 -  read# = readInt32OffAddr#
 -  write# = writeInt32OffAddr#
 -instance Prim I16# where
 -  index# = indexInt16OffAddr#
 -  read# = readInt16OffAddr#
 -  write# = writeInt16OffAddr#
 -instance Prim I8# where
 -  index# = indexInt8OffAddr#
 -  read# = readInt8OffAddr#
 -  write# = writeInt8OffAddr#
 -}
