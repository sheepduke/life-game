#!/usr/bin/env stack
{- stack script --resolver lts-14.20
--package random
--package array
--package split
-}
import Data.Array
import Data.List
import Data.List.Split
import qualified System.Environment as Env
import System.Random

type Matrix = Array (Int, Int) Bool

type Coordinate = (Int, Int)

data Board =
  Board Matrix Int

instance Show Board where
  show (Board matrix size) =
    unlines . map unwords . chunksOf size $
    [ if (matrix ! (row, col))
      then "X"
      else "-"
    | row <- [0 .. size - 1]
    , col <- [0 .. size - 1]
    ]

makeBoard :: Int -> Board
makeBoard size =
  Board
    (array
       ((0, 0), (size - 1, size - 1))
       [((x, y), False) | x <- [0 .. size - 1], y <- [0 .. size - 1]])
    size

placeSeeds :: Board -> Int -> IO Board
placeSeeds board@(Board _ size) seedsCount
  | size * size <= seedsCount = error "Too many initial seeds!"
  | otherwise = do
    genX <- newStdGen
    genY <- newStdGen
    let coordinates =
          nub $ zip (randomRs (0, size - 1) genX) (randomRs (0, size - 1) genY)
    let seeds = take seedsCount coordinates
    return $ foldl (\acc coor -> setCell acc coor True) board seeds

getCell :: Board -> Coordinate -> Bool
getCell (Board matrix size) coordinate
  | isValid size coordinate = matrix ! coordinate
  | otherwise = False

isValid :: Int -> Coordinate -> Bool
isValid size (row, col) = row >= 0 && row < size && col >= 0 && col < size

setCell :: Board -> Coordinate -> Bool -> Board
setCell (Board matrix size) coordinate value =
  Board (matrix // [(coordinate, value)]) size

countNeighbors :: Board -> Coordinate -> Int
countNeighbors board@(Board matrix size) (row, col) =
  length . filter (== True) . map (getCell board) . filter (/= (row, col)) $
  [(x, y) | x <- [row - 1 .. row + 1], y <- [col - 1 .. col + 1]]

willBeAlive :: Board -> Coordinate -> Bool
willBeAlive board coordinate =
  let neighborCount = countNeighbors board coordinate
   in neighborCount == 3 || (neighborCount == 2 && getCell board coordinate)

evolve :: Board -> Board
evolve board@(Board _ size) =
  foldl
    (\this coordinate -> setCell this coordinate $ willBeAlive board coordinate)
    board
    [(x, y) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

runFor :: Board -> Int -> IO Board
runFor board 0 = return board
runFor board times = do
  let newBoard = evolve board
  -- putStrLn . show $ newBoard
  runFor newBoard (times - 1)

run :: Int -> Int -> IO Board
run size seedsCount = do
  board <- placeSeeds (makeBoard size) seedsCount
  runFor board 1000

main :: IO ()
main = do
  (size:seedsCount:[]) <- Env.getArgs
  board <- run (read size :: Int) (read seedsCount :: Int)
  putStrLn . show $ board
  return ()
