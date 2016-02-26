import Text.Pandoc.Error
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
--Level one headers get their own slide, followed by a horizontal rule.
extractSlides (Header n m xs)
  | n==1 = [(Header n m xs),HorizontalRule]
  | otherwise = [Null]
--Anything in a CodeBlock is treated as a slide: the previously-ignored markdown is parsed and 
--assembled into a pandoc document.
extractSlides (CodeBlock attr string) =
  pullBlocks (readMarkdown def string)
--All other text is skipped
extractSlides x = []

pullBlocks :: Either Text.Pandoc.Error.PandocError Pandoc -> [Block]

--Each slide is read as a Pandoc "document," and then images are corrected and it's fed back with a horizontal rule
--to separate it from the other slides in the deck.
pullBlocks  (Right (Pandoc meta blocks)) = do
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
-- For the time being, reveal.js will launch links *inside* the window. This is nice, so I do it for all links.
-- Note it has the unfortunate side-effect of stripping formatting from the link text.
fancyLink (Link textbits (url,title)) = do
  let newlink = "<a href=\"" ++ url ++ "\" data-preview-link>" ++ (stringify textbits) ++ "</a>"
  RawInline (Format "html") newlink
  
fancyLink x = x


makeIframe :: String -> Inline
-- Iframes are arbitrarily defined at 600px tall, because they seem to break when scaling by percent.
makeIframe target = do
  let iframe = "<iframe allowfullscreen width=95% height=600px src=\"" ++ target ++ "\"></iframe>"
  RawInline (Format "html") iframe

fiximages :: Block -> Block
-- Images and Iframes that occupy a whole paragraph on their own are reformatted.
-- null list handling for images: just return the thing.
fiximages (Para [Image [] target]) = (Para [Image [] target])
-- an initial ">" before the link target denotes presenting it as an iframe, not an image.
fiximages (Para [Image text ('>':target,_)]) = Div nullAttr [Para text, Plain [(makeIframe target)]]

-- In general, image titles are dropped above the images and the image when clicked expands to fulscreen.
fiximages (Para [Image text target]) = Div nullAttr [Para text, Para [Link [Image [] target] target]]

-- Anything else is just itself.
fiximages x = x


slideReturn :: Either Text.Pandoc.Error.PandocError Pandoc -> Pandoc
-- extractSlides is a function, not a query, b/c it takes the format Block->[Block].
--This could and should be changed by just wrapping it all in a div element.
slideReturn (Right (Pandoc meta blocks)) = do
  let newData = foldl (++) [] (map extractSlides blocks)
  Pandoc meta newData

readDoc :: String -> Either Text.Pandoc.Error.PandocError Pandoc
readDoc = readJSON def

writeDoc :: Pandoc -> String
writeDoc = writeJSON def

myHTML :: Pandoc -> String
myHTML = writeHtmlString def

main :: IO ()
main = interact (writeDoc . slideReturn . readDoc)
