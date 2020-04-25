#!/usr/bin/env stack
{- stack script --resolver lts-14.20
--package random
--package unordered-containers
-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashSet as Set
import Data.List
import qualified System.Environment as Env
import qualified System.Random as Rand

type Position = (Int, Int)

type Cells = Set.HashSet Position

data Board =
  Board
    { size :: Int
    , cells :: Cells
    }

instance Show Board where
  show (Board {size, cells}) =
    unlines . map unwords $
    [ [ if Set.member (x, y) cells
      then "O"
      else "-"
    | y <- [1 .. size]
    ]
    | x <- [1 .. size]
    ]

makeBoard :: Int -> [Position] -> Board
makeBoard size cells = Board {size, cells = Set.fromList cells}

randomPositions :: Int -> Int -> IO [Position]
randomPositions size seedsCount = do
  genX <- Rand.newStdGen
  genY <- Rand.newStdGen
  return . take seedsCount . nub $
    zip (Rand.randomRs (1, size) genX) (Rand.randomRs (1, size) genY)

getCell :: Board -> Position -> Bool
getCell (Board {cells}) position = Set.member position cells

countNeighbors :: Board -> Position -> Int
countNeighbors board (x, y) =
  length . filter (\pos -> getCell board pos) $
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x, y - 1)
  , (x, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]

willBeAlive :: Board -> Position -> Bool
willBeAlive board position =
  let neighborCount = countNeighbors board position
   in (neighborCount == 2 && getCell board position) || neighborCount == 3

evolve :: Board -> Board
evolve board@(Board {size, cells}) =
  let newCells =
        Set.fromList . filter (\pos -> willBeAlive board pos) $
        [(x, y) | x <- [1 .. size], y <- [1 .. size]]
   in Board {size, cells = newCells}

runFor :: Board -> Int -> IO Board
runFor board 0 = return board
runFor board times = do
  let newBoard = evolve board
  -- putStrLn . show $ newBoard
  runFor newBoard (times - 1)

run :: Int -> Int -> IO Board
run size seedsCount = do
  cells <- randomPositions size seedsCount
  let board = makeBoard size cells
  runFor board 100000

main = do
  (size:seedsCount:[]) <- Env.getArgs
  board <- run (read size :: Int) (read seedsCount :: Int)
  putStrLn . show $ board
  return ()
