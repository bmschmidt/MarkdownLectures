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

--Level one headers get their own slide.

extractSlides (Header n m xs)
  | n==1 = [(Header 1 m xs)]
  | otherwise = []

-- Divs of class 'slide' are expanded into their contents,
-- with a slidebreak delimiter at the end.

extractSlides (Div (id, classes, meta) contents)
  | "slide" `elem` classes = add_header (Div (id, classes, meta) contents)
  | otherwise = []
  where content = Div (id, classes, meta) contents

-- standalone images (and iframes) are automatically turned into slides.
        
extractSlides (Para [Image attr text (target_1, target_2)]) =  
  fiximages (Para [Image attr text (target_1, target_2)])

--All other text is skipped
extractSlides x = []

-- Drop an empty level two header as a fake slide start.

add_header :: Block -> [Block]
add_header (Div attr contents) =
  [Header 2 attr [], Div nullAttr contents]
add_header x = [Header 2 nullAttr [], x]


fiximages :: Block -> [Block]
-- Images and Iframes that occupy a whole paragraph on their own are reformatted.
-- an initial ">" before the link target denotes presenting it as an iframe, not an image.
-- More recently, pandoc seems to encodeurl '>' as '%3E'; keeping the old pattern just in case.
fiximages (Para [Image attr text ('>':target,_)]) =
  add_header (Div attr [Para text, Plain [(makeIframe target)]])
 
fiximages (Para [Image attr text ('%':'3':'E':target, xs)]) =
  fiximages (Para [Image attr text ('>':target, xs)])

fiximages (Para [Image attr text (target_1, target_2)]) = do
--  let myimage =[Image nullAttr [] (target_1, target_2)]
--  let newlink = fancyLink $ Link nullAttr myimage (target_1, target_2)
--  let title = fancyLink $ Link nullAttr text (target_1, target_2)
--  Div nullAttr [Para [title], Para [newlink]]
--  let divAttr = ([], [], [("data-background-image",target_1),("data-background-size","contain")])
  let image_header = Header 2 ([], [], [("data-background-image",target_1),("data-background-size","contain")]) []
  let imageText = Plain [Span (boxenate attr) text]
  [image_header, imageText]
  
-- Anything else is just itself.
fiximages x = [x]



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

boxenate :: Attr -> Attr
boxenate (id, classes, keyvals) =
  (id, ("attribution":classes), keyvals)

slideReturn :: Pandoc -> Pandoc


slideReturn (Pandoc meta blocks) = do
  let slides = query extractSlides blocks
  let newData = walk fancyLink $ slides
--  let newData = walk fiximages $ walk fancyLink $ slides  
  Pandoc meta newData


main :: IO()
main = toJSONFilter slideReturn
