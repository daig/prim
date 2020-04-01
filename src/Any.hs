module Any (module Any, seq) where
import qualified Array

fromAddr :: Addr -> (# a #)
fromAddr = addrToAny#

-- | Must be run on an evaluated value, not a thunk
toAddr# :: a -> IO Addr
toAddr# = anyToAddr#

unpackClosure :: a -> (# Addr, Array.Byte, Array.Boxed b #)
unpackClosure = unpackClosure# 

getApStackVal :: a -> I64 -> (# I64, b #)
getApStackVal = getApStackVal#
