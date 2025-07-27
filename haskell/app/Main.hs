module Main where

import qualified MyLib (someFunc)
import qualified Game.Player as Pl
import qualified Game.BoardHouse as Bh 
import qualified Game.Board as Gb

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
