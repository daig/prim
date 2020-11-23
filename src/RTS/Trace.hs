{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module RTS.Trace where
import qualified P
import Prelude hiding  IO#)
import GHC.Types  IO#)

event ∷ P.Byte → ST_# s
event = traceEvent#

binaryEvent ∷ P.Byte → I {- ^ length -} → ST_# s
binaryEvent = traceBinaryEvent#
