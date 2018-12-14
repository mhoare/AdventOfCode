import System.IO
import Data.Maybe
import qualified Data.Set as Set

partOne = do
      x <- openFile "stars.txt" ReadMode
      cont <- hGetContents x
      let fLines = map (dropWhile (=='+')) (lines cont)
      print (sum (map read fLines))


-- Part Two

partials :: [Int] -> [Int]
partials = scanl1 (+) . cycle

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate = flip firstDuplicate' Set.empty

firstDuplicate' :: [Int] -> Set.Set Int -> Maybe Int
firstDuplicate' [] _ = Nothing
firstDuplicate' (x:xs) set
    | Set.member x set = Just x
    | otherwise        = firstDuplicate' xs (Set.insert x set)

partTwo = do
      x <- openFile "partTwo.txt" ReadMode
      cont <- hGetContents x
      let fLines = map (dropWhile (=='+')) (lines cont)
      print $ firstDuplicate (partials (map read fLines))
