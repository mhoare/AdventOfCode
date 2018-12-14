import System.IO
import Data.Maybe
import Data.List
import Data.Tuple


-- Part One - Inventory Management

checksum :: [String] -> Int
checksum xs = length twos * length threes
    where countMap = charCount xs
          twos     = filter id $ map exactlyTwo countMap
          threes   = filter id $ map exactlyThree countMap

charCount :: [String] -> [[(Char, Int)]]
charCount xs = map (map (\x -> (head x, length x)) . group . sort) xs

exactlyTwo :: [(Char, Int)] -> Bool 
exactlyTwo xs = length (filter ((==2).snd) xs) > 0

exactlyThree :: [(Char, Int)] -> Bool
exactlyThree xs = length ( filter ((==3).snd) xs ) > 0

partOne = checksum

-- Part Two

offByOne :: Eq a => [a] -> [a] -> Maybe [a]
offByOne (x:xs) (y:ys)
    | x  == y  = (x :) <$> offByOne xs ys
    | xs == ys = Just xs 
offByOne _ _ = Nothing

partTwo l = (head [ r | (x:xs) <- tails l, y <- xs, Just r <- [offByOne x y]])

main = do
      x <- openFile "inputPartOne.txt" ReadMode
      cont <- hGetContents x
      print $ partOne (lines cont)
      print $ partTwo (lines cont)
