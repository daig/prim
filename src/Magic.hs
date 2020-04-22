module Magic (module X) where
import GHC.Magic as X (inline,noinline,lazy,oneShot)
