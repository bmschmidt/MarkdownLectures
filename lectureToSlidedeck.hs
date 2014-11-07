import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Builder

--

--Functions to first build up a new document consisting of 
--all the header blocks or quote blocks. To be combined into a new
--doc.

extractSlides :: Block -> [Block]
extractSlides (Header n m xs)
  | n==1 = [(Header n m xs),HorizontalRule]
  | otherwise = [Null]
extractSlides (CodeBlock attr string) =
  pullBlocks (readMarkdown def string)
extractSlides x = []

pullBlocks :: Pandoc -> [Block]
pullBlocks (Pandoc meta blocks) = (walk fiximages blocks) ++ [HorizontalRule]


makeIframe :: String -> Inline
makeIframe target = do
  let iframe = "<iframe allowfullscreen width=95% height=600px src=\"" ++ target ++ "\"></iframe>"
  RawInline (Format "html") iframe

fiximages :: Block -> Block
fiximages (Para [Image [] target]) = (Para [Image [] target])

-- an initial ">" before the link target denotes presenting it as an iframe, not an image.
fiximages (Para [Image text ('>':target,_)]) = Div nullAttr [Para text, Plain [(makeIframe target)]]

fiximages (Para [Image text target]) = Div nullAttr [Para text, Para [Image [] target]]
fiximages x = x


slideReturn :: Pandoc -> Pandoc
slideReturn (Pandoc meta blocks) = do
  let newData = foldl (++) [] (map extractSlides blocks)
  Pandoc meta newData
--Very broken-down functions to actually read and write.

readDoc :: String -> Pandoc
readDoc = readJSON def

writeDoc :: Pandoc -> String
writeDoc = writeJSON def

main :: IO ()
main = interact (writeDoc . slideReturn . readDoc)
