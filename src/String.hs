module String where
import qualified GHC.Types

-- | Null terminated C-style Strings
type C# = Addr#

-- | Boxed Linked list of @Char@
type List = [GHC.Types.Char]
