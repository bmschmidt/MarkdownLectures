import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.Builder

--Functions to first build up a new document consisting of 
--all the header blocks or all the bold entries outside of headers.
--These will be recombined into a new document. 

extractBolds :: [Block] -> [Block]
extractBolds x = foldl (++) [] (map extractHeaderOrBold x)

extractHeaderOrBold :: Block -> [Block]
extractHeaderOrBold (Header n m xs) = [Header n m xs]
extractHeaderOrBold x = query extractBold x

extractBold :: Inline -> [Block]
extractBold (Strong xs) = [Para xs]
extractBold x = []

--List junk--probably are better ways to do this, but haskell is hard.
--First, adding a paragraph to the end of a block of blocks
--(the basic element in an outline)

addParaToEnd :: [[Block]] -> Block -> [[Block]]
addParaToEnd [] b = [[b]] 
addParaToEnd a b = do
  let end = last a
  let start = init a
  let newend = reverse $ addParaToFront (reverse end) b
  start ++ [newend]


-- Patterning matching on the end is difficult, so we actually add to the 
-- front of a reversed list.

addParaToFront :: [Block] -> Block -> [Block]
-- Adding a para to a nested list should drop down the nest.
addParaToFront ((OrderedList attr sublist):xs) newblock = do
    let newList = OrderedList attr $ addParaToEnd sublist newblock
    newList:xs
addParaToFront (x:xs) newblock = do
  newblock:x:xs
addParaToFront _ newblock = [newblock]

-- Here's another goofy first-last pairing for adding outlines

ensureOutlineAtFront :: [Block] -> [Block]
--Make sure the first element is an outline.
ensureOutlineAtFront ((OrderedList attr xbs):xs) = ((OrderedList attr xbs):xs)
ensureOutlineAtFront xs = (emptyOrderedList):xs

ensureOutlineAtEnd :: [Block] -> [Block]
ensureOutlineAtEnd [] = [emptyOrderedList]
ensureOutlineAtEnd xs = reverse $ ensureOutlineAtFront $ reverse xs


--lots of patterning matching to avoid adding to lists, and to ensure that 
--we generate a new numbered outline where necessary.

-- This is the actual action; it drops a block into the right position in
-- an outline, and is continually called via a `foldl` function.

--If the header level is greater than 1, it recurses on itself adding 
--a new outline, if necessary, to the last element in the current outline.

addToOutline :: Block -> Block -> Block
addToOutline (OrderedList attr xs) (Header n hattr headerlines)
   | n > 1 && null xs = do
       OrderedList attr [[OrderedList attr [[addToOutline emptyOrderedList lowerHeader]]]]
   | n > 1 = do
     let oldpart = init xs
     let lastelement = ensureOutlineAtEnd (last xs)
     let firstlast = init lastelement
     let lastlast = last lastelement
     let newLastLast = addToOutline lastlast lowerHeader
     let newLastBlock = firstlast ++ [newLastLast]
     OrderedList attr (oldpart ++ [newLastBlock])
   | n == 1 = OrderedList attr (xs ++ [[(Para headerlines)]])
  where
    lowerHeader = Header (n-1) hattr headerlines
    
-- The basic appender takes either an OrderedList 
-- or a header; different behaviors for each one.

appendOutline :: Block -> Block -> Block
appendOutline (OrderedList attr xs) (Para xbs) = do
  OrderedList attr $ addParaToEnd xs (Para [(Strong xbs)])
appendOutline oldlist newheader = do
  addToOutline oldlist newheader

-- Convenience function for building an ordered pandoc list with default settings
emptyOrderedList = OrderedList ( 1 , DefaultStyle , DefaultDelim ) []

-- The overall action: first build out the list of bolds and headers,
-- and then fold them all together into a new outline,
-- and finally return a new Pandoc dcoument consisting of just that outline.

outlineReturn :: Pandoc -> Pandoc
outlineReturn (Pandoc meta blocks) = do
  let newData = foldl appendOutline emptyOrderedList (extractBolds blocks)
  Pandoc meta [newData]

--Very broken-down functions to actually read and write.

main = toJSONFilter outlineReturn
