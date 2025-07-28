module MyLib where

import Game.Ranking (salvarVencedor, salvarDerrotado, mostrarRanking)
import Game.GameLoop (playTurn)
import Game.GameState (GameState(..), getCurrentPlayer, players)
import Game.Player (name, playerId)

loopJogo :: GameState -> IO ()
loopJogo gs = do
  if length (players gs) <= 1 then do
    let vencedor = head (players gs)
    putStrLn $ "\n🏆 " ++ name vencedor ++ " venceu o jogo!"
    
    -- Salvar estatísticas no ranking
    salvarVencedor vencedor
    mapM_ salvarDerrotado (filter ((/= playerId vencedor) . playerId) (players gs))
    
    -- Mostrar ranking final
    putStrLn "\n📊 Ranking atualizado:"
    mostrarRanking
  else do
    novoEstado <- playTurn gs
    loopJogo novoEstado
