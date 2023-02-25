--------------------------------------------------------------------
-- | Description : Exception CallStacks for debugging
--------------------------------------------------------------------
module IO.Exception.Stack (CallStack(CallStack),HasCallStack,prettyCallStack,SrcLoc(..),prettySrcLoc) where
import GHC.Stack.Types
import GHC.Stack as X

pattern CallStack ∷ [([Char],SrcLoc)] → CallStack
pattern CallStack s ← (getCallStack → s) where CallStack = fromCallSiteList
