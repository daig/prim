module Any (module Any, seq) where
import qualified Ref
import qualified Array

fromRef :: Ref.Byte -> (# a #)
fromRef = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toRef# :: a -> IO Ref.Byte
toRef# = anyToAddr#

unpackClosure :: a -> (# Ref.Byte, Array.Byte, Array.Boxed b #)
unpackClosure = unpackClosure# 

getApStackVal :: a -> I64 -> (# I64, b #)
getApStackVal = getApStackVal#
