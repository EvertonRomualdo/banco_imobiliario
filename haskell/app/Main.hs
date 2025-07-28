module Main where

import qualified Game.Player as Pl
import qualified Game.Board as Gb
import qualified Game.BoardHouse as Bh
import Game.GameState
import Game.GameLoop
import Game.Ranking (salvarVencedor, salvarDerrotado, mostrarRanking)

main :: IO ()
main = do
  putStrLn "🏁 Bem-vindo ao Banco Imobiliário Terminal!"
  loopJogo estadoInicial

-- Jogadores iniciais
jogadores :: [Pl.Player]
jogadores =
  [ Pl.Player 1 "Alice" 0 1000 0 []
  , Pl.Player 2 "Bob"   0 1000 0 []
  ]

-- Tabuleiro gerado com 20 casas
tabuleiroInicial :: Gb.Board
tabuleiroInicial =
  let casas = Gb.gerarCasas
  in Gb.Board
      "Banco Imobiliário Terminal"
      "Versão de 20 casas, com imposto, prisão e especiais"
      (head casas)
      1000  -- saldo inicial
      200   -- bônus por passar na casa inicial
      casas

-- Estado inicial do jogo
estadoInicial :: GameState
estadoInicial = GameState jogadores (Gb.housesOnTheBoard tabuleiroInicial) 0 20 0


-- Loop principal
loopJogo :: GameState -> IO ()
loopJogo gs = do
  if length (players gs) <= 1 then do
    let vencedor = head (players gs)
    putStrLn $ "\n🏆 " ++ Pl.name vencedor ++ " venceu o jogo!"

    -- Atualiza o ranking
    salvarVencedor vencedor
    mapM_ salvarDerrotado (filter ((/= Pl.playerId vencedor) . Pl.playerId) (players gs))

    -- Mostra ranking final
    putStrLn "\n📊 Ranking atualizado:"
    mostrarRanking
  else do
    novoEstado <- playTurn gs
    loopJogo novoEstado
