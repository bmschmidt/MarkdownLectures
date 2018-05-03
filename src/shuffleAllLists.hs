import Text.Pandoc.Error
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Text.Pandoc.JSON
import Text.Pandoc.Builder
import System.Random
import System.IO.Unsafe (unsafePerformIO)

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
  i <- System.Random.randomRIO (0, length(x)-1)
  r <- shuffle (take i x ++ drop (i+1) x)
  return (x!!i : r)

shuffleList :: Block -> Block
shuffleList (OrderedList attr xs) = (OrderedList attr (unsafePerformIO (shuffle xs)))
shuffleList x = x

shuffleLists :: Pandoc -> Pandoc
shuffleLists = walk shuffleList


main :: IO ()
main = toJSONFilter shuffleLists
