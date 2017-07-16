module Lib
    ( Cell(Cell, Indent)
    , cellToChar
    , formatGrid
    , outputGrid
    , zipOverGrid
    , zipOverGridWith
    , coordsGrid
    , gridWithCoords
    , findWordInLine
    , findWordInCellLinePrefix
    , skew
    , getLines
    , findWord
    , findWords
    ) where

import Data.List (isInfixOf, transpose, zipWith)
import Data.Maybe (catMaybes, listToMaybe)

type Grid a = [[a]]

data Cell = Cell (Integer, Integer) Char
          | Indent
          deriving (Eq, Ord, Show)

cellToChar :: Cell -> Char
cellToChar (Cell _ c) = c
cellToChar Indent = '?'

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cellToChar

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = Indent : line

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

getLines :: Grid Cell -> Grid Cell
getLines grid = 
  let horizontal = grid
      vertical = transpose grid
      diagonal = diagonalize grid
      altDiagonal = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal ++ altDiagonal
  in lines ++ map reverse lines

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing -> findWordInLine word (tail line)
    cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs)
  | x == cellToChar c = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords
