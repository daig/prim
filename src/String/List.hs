{-# language UnliftedFFITypes #-}
module String.List where
import Prelude hiding (IO)
import Stock.IO
import qualified Stock.Char as Stock
import A
import A.Prim

type S = [Stock.Char]

debugLn ∷ S → IO ()
debugLn xs = IO \ s → case pack xs s of
                 (# s', mba #) → case c_debugLn mba of IO f → f s'

debugErrLn ∷ S → IO ()
debugErrLn xs = IO \ s → case pack xs s of
                    (# s', mba #) → case c_debugErrLn mba of IO f → f s'

foreign import ccall unsafe "debugLn"
    c_debugLn ∷ MA (☸) Char → IO ()

foreign import ccall unsafe "debugErrLn"
    c_debugErrLn ∷ MA (☸) Char → IO ()

pack ∷ S → IO# (MA (☸) Char)
pack xs s0 = -- Start with 1 so that we have space to put in a \0 at
              -- the end
              case len 1# xs of
              l →
                  case new# l s0 of
                  (# s1, mba #) →
                      case write mba 0# xs s1 of
                      s2 → (# s2, mba #)
    where len l [] = l
          len l (_ : xs') = len (l +# 1#) xs'

          write ∷ MA s Char → I → S → ST_# s
          write mba offset [] s = write# mba offset '\0'# s
          write mba offset (Stock.C# x : xs') s
              = case write# mba offset x s of
                s' →
                    write mba (offset +# 1#) xs' s'
