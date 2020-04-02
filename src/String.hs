module String where
import qualified GHC.Types

-- | Null terminated C-style Strings
type C# = Addr#

-- | Boxed Linked list of @Char@
type Linked = [GHC.Types.Char]
