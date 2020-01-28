import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.JSON
import Data.String

-- Converts from the old format to the new one.

reformat :: Block -> Block
reformat (Div (id, "slide":classes, attr) blocks) =
  BlockQuote blocks  
reformat x = x

dropImages :: Inline -> Inline
dropImages (Image attr text (target_1, target_2)) =
  Span attr text
dropImages x = x


change (Pandoc meta blocks) = do
  let newblocks = walk dropImages $ walk reformat $ blocks
  Pandoc meta newblocks
  
main :: IO()
main = toJSONFilter change
