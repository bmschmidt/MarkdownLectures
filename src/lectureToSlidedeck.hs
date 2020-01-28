--import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.JSON
import Text.Pandoc.Pretty (text)
import Data.Text (pack,unpack)
import qualified Data.Text as T
--Just for Bookworm.
import Network.HTTP.Base (urlEncode)


--Functions to first build up a new document consisting of 
--all the header blocks or quote blocks. To be combined into a new
--doc.
extractSlides :: Block -> [Block]

--Level one headers get their own slide, followed by a horizontal rule.
--All slides, in general, are followed by a Horizontal rule to ensure blocks don't run into each other.

extractSlides (Header n m xs)
  | n==1 = [(Header 1 m xs), HorizontalRule]
  | otherwise = []

-- Divs of class 'slide' are expanded into their contents,
-- with a slidebreak delimiter at the end.

extractSlides (Div (id, classes, meta) contents)
  | "slide" `elem` classes = contents ++ [HorizontalRule]
  | otherwise = []
  where content = Div (id, classes, meta) contents


-- standalone images (and iframes) are automatically turned into slides.
        
extractSlides (Para [Image attr text (target_1, target_2)]) = 
  [fiximages (Para [Image attr text (target_1, target_2)]), HorizontalRule]

--All other text is skipped
extractSlides x = []


-- This is just for my personal use. Shouldn't affect anyone else.
addBookwormLinks :: Block -> Block
addBookwormLinks (CodeBlock (codeblock,["bookworm"],keyvals) code) = do
  let block = (CodeBlock (codeblock,["bookworm"],keyvals) code)
  let target = "http://benschmidt.org/BookwormD3/#" ++ (urlEncode code)
  let target = "http://benschmidt.org/D3/#" ++ (urlEncode code)
  let link = Para [Link nullAttr [Str "View"] (target,"")]
  Div nullAttr [block,link]
--addBookwormLinks (RawBlock _ _) = Null
addBookwormLinks x = x

fancyLink :: Inline -> Inline
-- For the time being, reveal.js will launch links *inside* the window. This is nice, so I do it for all links.
-- Note it has the unfortunate side-effect of stripping formatting from the link text.

fancyLink (Link (id,classes,attr) textbits link) = do
  Link (id,classes,attr ++ [("data-preview-link","false")]) textbits link
fancyLink x = x

makeIframe :: String -> Inline


-- data-src instead of 'src' for images causes lazy-loading.
resrc :: (String, String) -> (String, String)
resrc ("src", x) = ("data-src", x)
resrc x = x

makeLazyLoad :: Attr -> Attr
makeLazyLoad (id, classes, kvpairs) =
  (id, classes, (map resrc kvpairs))


-- Iframes are arbitrarily defined at 600px tall, because they seem to break when scaling by percent.
makeIframe target = do
  let iframe = "<iframe allowfullscreen width=95% height=600px data-src=\"" ++ target ++ "\" data-autoplay></iframe>"
  RawInline (Format "html") iframe

fiximages :: Block -> Block
-- Images and Iframes that occupy a whole paragraph on their own are reformatted.
-- an initial ">" before the link target denotes presenting it as an iframe, not an image.
-- More recently, pandoc seems to encodeurl '>' as '%3E'; keeping the old pattern just in case.
fiximages (Para [Image attr text ('>':target,_)]) =
  Div attr [Para text, Plain [(makeIframe target)]]
 
fiximages (Para [Image attr text ('%':'3':'E':target,_)]) =
  Div attr [Para text, Plain [(makeIframe target)]]

fiximages (Para [Image attr [] (target_1, target_2)]) =
  Header 2 ([],[],[("data-background-image",target_1),("data-background-size","contain")]) []
  -- Don't change until the fullscreen works again.
--  Para [Image attr [] (target_1, target_2)]
  
-- Putting a period as the text does the same thing--back compatibility.
fiximages (Para [Image attr [(Str ".")] (target_1, target_2)]) = do
  Header 2 ([],[],[("data-background-image",target_1),("data-background-size","contain")]) []

fiximages (Para [Image attr text (target_1, target_2)]) = do
  let myimage =[Image nullAttr [] (target_1, target_2)]
  let newlink = fancyLink $ Link nullAttr myimage (target_1, target_2)
  let title   = fancyLink $ Link nullAttr text (target_1, target_2)
--  Div nullAttr [Para [title], Para [newlink]]
  Header 2 ([], [], [("data-background-image",target_1),("data-background-size","contain")]) [Span attr text]

-- Anything else is just itself.
fiximages x = x

slideReturn :: Pandoc -> Pandoc

-- Should probably be a foldl, but I forget how.

removeUnneededBars :: [Block] -> [Block]

removeUnneededBars (HorizontalRule:Header n m x:xs) =
  (Header n m x):removeUnneededBars(xs)

removeUnneededBars (x:y:xs) =
  x:removeUnneededBars(y:xs)

removeUnneededBars [x] =
  [x]

removeUnneededBars [] =
  []

slideReturn (Pandoc meta blocks) = do
  let slides = query extractSlides blocks
  let newData = removeUnneededBars $ walk fiximages $ walk fancyLink $ slides
--  let newData = walk fiximages $ walk fancyLink $ slides  
  Pandoc meta newData


main :: IO()
main = toJSONFilter slideReturn
