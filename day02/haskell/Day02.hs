module Day02 (day02) where

import Data.List (group, sort, find)
import qualified Data.Set as S

readInputFile :: IO String
readInputFile = readFile "../input.txt"

-- combine letters into groups and take their lengths.
-- then check the numbers 2 and 3 are part of the resulting list
has2Or3Chars :: String -> (Bool, Bool)
has2Or3Chars str = (2 `elem` letterCounts, 3 `elem` letterCounts)
  where letterCounts = map length . group . sort $ str

--  get a list of bool pairs for whether the id has 2 and 3 duplicate characters
--  count the the number of True values for each and multiply them
part1 :: String -> Int
part1 input = (count twos) * (count threes)
  where (twos, threes) = unzip . map has2Or3Chars . lines $ input
        count = length . filter (== True)

-- sort ids so similar ids are next to each other in the list
-- loop over each pair to find find if they are only off by one character
-- then extract the common letters
part2 :: String -> Maybe String
part2 input = fmap getCommonLetters' . find isOffByOne' $ toPairs ids
  where ids = lines $ input
        getCommonLetters' = uncurry getCommonLetters
        isOffByOne' = uncurry isOffByOne

-- pair each element in the list with every other element to compare against
toPairs :: [String] -> [(String, String)]
toPairs (x:xs) = zip (repeat x) xs ++ toPairs xs

-- PRECONDITION: There is only one mismatched character
-- take all letters before the and after the mismatched character
getCommonLetters :: String -> String -> String
getCommonLetters id1 id2 = before ++ after
  where before = fst . unzip . takeWhile (uncurry (==)) $ zipped
        after  = fst . unzip . tail . dropWhile (uncurry (==)) $ zipped
        zipped = zip id1 id2

-- drop until we find a mismatched character and then check that the tail is equal
isOffByOne :: String -> String -> Bool
isOffByOne id1 id2 = (uncurry (==)) . unzip . tail . dropWhile (uncurry (==)) $ zip id1 id2

day02 :: IO ()
day02 = do
  input <- readInputFile

  putStrLn . show . sort . lines $ input
  putStrLn . show . part1 $ input
  putStrLn . show . part2 $ input

