module Lib
    ( Cell(Cell, Indent)
    , Game(gameGrid, gameWords)
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
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    ) where

import Data.List (isInfixOf, transpose, zipWith)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as Dict

type Grid a = [[a]]

data Cell = Cell (Integer, Integer) Char
          | Indent
          deriving (Eq, Ord, Show)

data Game = Game
            { gameGrid :: (Grid Cell),
              gameWords :: (Dict.Map String (Maybe [Cell]))
            }
          deriving Show

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

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = Dict.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . Dict.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . Dict.elems $ gameWords game

playGame :: Game -> String -> Game
playGame game word | not $ Dict.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
    Nothing -> game
    Just cs ->
        let dict = gameWords game
            newDict = Dict.insert word foundWord dict
        in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game@(Game grid dict) = formatGrid grid
                                   ++ "\n\n"
                                   ++ (show $ score game)
                                   ++ "/"
                                   ++ (show $ totalWords game)

completed :: Game -> Bool
completed game = score game == totalWords game
