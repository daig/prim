module Exception (module Exception, raise#) where

catch :: IO a -> (b -> IO a) -> IO a
catch = catch#

raiseIO :: a -> IO b
raiseIO = raiseIO#
