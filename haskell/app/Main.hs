module Main where

import qualified MyLib (someFunc)
import qualified Game.Player as Pl
import qualified Game.BoardHouse as Bh 
import qualified Game.Board as Gb
import qualified Game.GameState as Gs
import qualified Game.Ui as Ui

main :: IO ()
main = do
  putStrLn "Bem vindo!"
  putStrLn (Ui.renderBoard iniciarEstado)

iniciarEstado :: Gs.GameState
iniciarEstado = Gs.GameState {
    Gs.players = [
        Pl.Player 1 "Ana" 2 1000 0 [],
        Pl.Player 2 "Pedro" 5 1500 0 []
    ],
    Gs.board = criarTabuleiro,
    Gs.currentPlayerIndex = 0,
    Gs.maxPosition = 20,
    Gs.turnCount = 1
}

criarTabuleiro :: [Bh.BoardHouse]
criarTabuleiro =
  [ Bh.BoardHouse i ("Casa" ++ show i) (tipo i) 200 100 50 100 25 50 0 0 100 100 False
  | i <- [0..19]
  ]
  where
    tipo i
      | i == 0 = "inicio"
      | i `elem` [3, 17] = "imposto"
      | i `elem` [4, 15] = "rolar"
      | i `elem` [6, 13] = "prisao"
      | i == 10 = "feriado"
      | otherwise = "cidade"