module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWordInLine
    , skew
    , getLines
    , findWord
    , findWords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn $ formatGrid grid

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

skew :: Grid -> Grid
skew [] = []
skew (x:xs) = x : skew (map indent xs)
  where indent line = '_' : line

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

getLines :: Grid -> [String]
getLines grid = 
  let horizontal = grid
      vertical = transpose grid
      diagonal = diagonalize grid
      altDiagonal = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal ++ altDiagonal
  in lines ++ map reverse lines

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

grid = ["__C________R___"
       ,"__SI________U__"
       ,"__HASKELL____B_"
       ,"__A__A_____S__Y"
       ,"__R___B___C____"
       ,"__PHP____H_____"
       ,"____S_LREP_____"
       ,"____I__M_Y__L__"
       ,"____L_E__T_O___"
       ,"_________HB____"
       ,"_________O_____"
       ,"________CN_____"
       ]

languages = ["BASIC"
            ,"COBOL"
            ,"CSHARP"
            ,"HASKELL"
            ,"LISP"
            ,"PERL"
            ,"PHP"
            ,"PYTHON"
            ,"RUBY"
            ,"SCHEME"
            ]
