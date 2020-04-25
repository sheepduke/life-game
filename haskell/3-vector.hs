#!/usr/bin/env stack
{- stack script --resolver lts-14.20
--package random
--package vector
--package split
--package unordered-containers
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashSet as Set
import Data.List
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UVec
import qualified System.Environment as Env
import System.Random

data Board =
  Board
    { size :: Int
    , matrix :: Vec.Vector (UVec.Vector Bool)
    }

type Position = (Int, Int)

type Cell = Bool

instance Show Board where
  show (Board {size, matrix}) =
    let content = map UVec.toList (Vec.toList matrix)
     in unlines . map (unwords . map cellToChar) $ content
    where
      cellToChar cell =
        if cell
          then "O"
          else "-"

makeRandomSeedsBoard :: Int -> Int -> IO Board
makeRandomSeedsBoard size seedsCount
  | size ^ 2 <= seedsCount = error "Too many initial seeds"
  | otherwise = do
    genX <- newStdGen
    genY <- newStdGen
    let positionSet =
          Set.fromList . take seedsCount . nub $
          zip (randomRs (0, size - 1) genX) (randomRs (0, size - 1) genY)
    let positionToCell row col =
          if Set.member (row, col) positionSet
            then True
            else False
    let matrix =
          Vec.generate
            size
            (\row -> UVec.generate size (\col -> positionToCell row col))
    return Board {size, matrix}

getCell :: Board -> Position -> Cell
getCell (Board {size, matrix}) (row, col)
  | row < 0 || row >= size || col < 0 || col >= size = False
  | otherwise = matrix `Vec.unsafeIndex` row `UVec.unsafeIndex` col

countNeighbors :: Board -> Position -> Int
countNeighbors board (row, col) =
  length . filter (== True) $
  [ getCell board (row - 1, col - 1)
  , getCell board (row - 1, col)
  , getCell board (row - 1, col + 1)
  , getCell board (row, col - 1)
  , getCell board (row, col + 1)
  , getCell board (row + 1, col - 1)
  , getCell board (row + 1, col)
  , getCell board (row + 1, col + 1)
  ]

willBeAlive :: Board -> Position -> Bool
willBeAlive board position =
  let neighborCount = countNeighbors board position
   in neighborCount == 3 || (neighborCount == 2 && getCell board position)

evolveOnce board@(Board {size}) =
  let matrix =
        Vec.fromList
          [ UVec.fromList
            [willBeAlive board (row, col) | col <- [0 .. size - 1]]
          | row <- [0 .. size - 1]
          ]
   in Board {size, matrix}

evolve :: Board -> Int -> Board
evolve board@(Board {size}) times
  | times <= 0 = board
  | otherwise = evolve (evolveOnce board) (times - 1)

main :: IO ()
main = do
  (size:seedsCount:[]) <- Env.getArgs
  board <- makeRandomSeedsBoard (read size :: Int) (read seedsCount :: Int)
  let finalBoard = evolve board 100000
  putStrLn . show $ finalBoard
  return ()
