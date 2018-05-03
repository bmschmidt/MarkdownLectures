import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.JSON
import Data.String

-- Converts from the old format to the new one.

reformat :: Block -> Block
reformat (CodeBlock (id,classes,attr) string) =
   Div (id, "slide":classes, attr) [RawBlock (Format "markdown") string]
reformat x = x


main :: IO()
main = toJSONFilter reformat
