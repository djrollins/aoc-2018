module Day01 (day01) where

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty ((:|)) )
import System.IO

inputData :: IO String
inputData = readFile "../input.txt"

applyChange :: String -> Int -> Int
applyChange (op:val) acc =
  case op of
    '+' -> acc + val'
    '-' -> acc - val'
    where val' = read val

part1 :: String -> Int
part1 = foldr applyChange 0 . lines

findDuplicate :: NonEmpty Int -> State (S.Set Int) Int
findDuplicate (x :| xs) = do

  sizeBefore <- gets S.size
  modify (S.insert x)
  after <- get

  if (S.size after > sizeBefore)
  then return $ evalState (findDuplicate . NE.fromList $ xs) after
  else return x


part2Simple :: String -> Int
part2Simple input = do
  evalState (findDuplicate . NE.scanl (flip applyChange) 0 . NE.cycle . NE.fromList . lines $ input) S.empty

-- overly complicated version part 2

isUnique :: Int -> State (S.Set Int) (Bool, Int)
isUnique new = do
  sizeBefore <- gets S.size
  modify (S.insert new)
  sizeAfter <- gets S.size
  return $ (sizeAfter > sizeBefore, new)

part2 :: String -> Int
part2 input =
    snd .
    head .
    dropWhile (fst) .
    evalState (traverse (isUnique) .
    scanl (flip applyChange) 0 . cycle . lines $ input) $ S.empty

day01 = do
  input <- inputData
  putStrLn "-- Day 01"
  putStr "part 1: "
  putStrLn . show . part1 $ input
  putStr "part 2: "
  putStrLn . show . part2 $ input
