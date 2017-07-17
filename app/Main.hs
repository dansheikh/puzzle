module Main where

import Lib
import Data
import System.IO

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word> "
  input <- getLine
  let newGame = playGame game input
  if completed newGame then
    putStrLn "Game Over!"
  else  
    playTurn newGame

main :: IO ()
main = do
  let game = makeGame grid languages
  hSetBuffering stdout NoBuffering
  playTurn game
