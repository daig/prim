module P.Fun (Fun, module X) where
import GHC.Ptr as X (FunPtr,nullFunPtr,castFunPtr,castFunPtrToPtr,castPtrToFunPtr)

type Fun = FunPtr

