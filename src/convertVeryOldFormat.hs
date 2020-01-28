import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.JSON
import Data.String

-- Converts from the old format to the new one.

reformat :: Block -> Block
reformat (BlockQuote blocks) =
   Div ("0", ["slide"], []) blocks
reformat x = x


main :: IO()
main = toJSONFilter reformat
