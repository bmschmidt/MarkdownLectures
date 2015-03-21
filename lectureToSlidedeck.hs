import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Shared (stringify)

--Just for Bookworm.
import Network.HTTP.Base (urlEncode)
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
pullBlocks (Pandoc meta blocks) = do
  let newblocks = walk fiximages $ walk fancyLink $ walk addBookwormLinks blocks
  (newblocks ++ [HorizontalRule])

addBookwormLinks :: Block -> Block
addBookwormLinks (CodeBlock (codeblock,["bookworm"],keyvals) code) = do
  let block = (CodeBlock (codeblock,["bookworm"],keyvals) code)
  let target = "http://benschmidt.org/beta/#" ++ (urlEncode code)
  let link = Para [Link [Str "View"] (target,"")]
  Div nullAttr [block,link]
addBookwormLinks (RawBlock _ _) = Null
addBookwormLinks x = x


fancyLink :: Inline -> Inline
fancyLink (Link textbits (url,title)) = do
  let newlink = "<a href=\"" ++ url ++ "\" data-preview-link>" ++ (stringify textbits) ++ "</a>"
  RawInline (Format "html") newlink
  
fancyLink x = x

makeIframe :: String -> Inline
makeIframe target = do
  let iframe = "<iframe allowfullscreen width=95% height=600px src=\"" ++ target ++ "\"></iframe>"
  RawInline (Format "html") iframe

fiximages :: Block -> Block
-- null list handling.
fiximages (Para [Image [] target]) = (Para [Image [] target])
-- an initial ">" before the link target denotes presenting it as an iframe, not an image.
fiximages (Para [Image text ('>':target,_)]) = Div nullAttr [Para text, Plain [(makeIframe target)]]
-- In general, image titles are dropped above the images and the image when clicked expands to fulscreen.
fiximages (Para [Image text target]) = Div nullAttr [Para text, Para [Link [Image [] target] target]]

-- Anything else is just itself.
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

myHTML :: Pandoc -> String
myHTML = writeHtmlString def

main :: IO ()
main = interact (writeDoc . slideReturn . readDoc)
