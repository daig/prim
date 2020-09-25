{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module RTS.Trace where
import qualified Ref
import Prelude hiding (IO)
import GHC.Types (IO)

event ∷ Ref.Byte → ST_ s
event = traceEvent#

binaryEvent ∷ Ref.Byte → I64 {- ^ length -} → ST_ s
binaryEvent = traceBinaryEvent#
