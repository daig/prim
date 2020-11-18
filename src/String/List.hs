{-# language UnliftedFFITypes #-}
module String.List where
import qualified GHC.Types

type S = [GHC.Types.Char]

debugLn ∷ S → GHC.Types.IO ()
debugLn xs = GHC.Types.IO (\s0 →
                 case mkMBA xs s0 of
                 (# s1, mba #) →
                     case c_debugLn mba of
                     GHC.Types.IO f → f s1)

debugErrLn ∷ S → GHC.Types.IO ()
debugErrLn xs = GHC.Types.IO (\s0 →
                    case mkMBA xs s0 of
                    (# s1, mba #) →
                        case c_debugErrLn mba of
                        GHC.Types.IO f → f s1)

foreign import ccall unsafe "debugLn"
    c_debugLn ∷ MutableByteArray# RealWorld → GHC.Types.IO ()

foreign import ccall unsafe "debugErrLn"
    c_debugErrLn ∷ MutableByteArray# RealWorld → GHC.Types.IO ()

mkMBA ∷ S → IO# (MutableByteArray# RealWorld)
mkMBA xs s0 = -- Start with 1 so that we have space to put in a \0 at
              -- the end
              case len 1# xs of
              l →
                  case newByteArray# l s0 of
                  (# s1, mba #) →
                      case write mba 0# xs s1 of
                      s2 → (# s2, mba #)
    where len l [] = l
          len l (_ : xs') = len (l +# 1#) xs'

          write mba offset [] s = writeCharArray# mba offset '\0'# s
          write mba offset (GHC.Types.C# x : xs') s
              = case writeCharArray# mba offset x s of
                s' →
                    write mba (offset +# 1#) xs' s'
