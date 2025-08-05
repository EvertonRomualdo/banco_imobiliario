module MyLib where

import Game.Ranking (salvarVencedor, salvarDerrotado, mostrarRanking)
import Game.GameLoop (playTurn)
import Game.Board

import Game.Player (name, playerId)
import qualified Game.Player as Pl
import qualified Game.BoardHouse as Bh
import qualified Game.Ranking


loopJogo :: Board -> IO ()
loopJogo gs = do

    --Verifica se ha um vencedor
    if length (players gs) <= 1 then do
        let vencedor = head (players gs)
        putStrLn $ "\n " ++ Pl.name vencedor ++ " venceu o jogo!"
        Game.Ranking.salvarVencedor vencedor
        mapM_ Game.Ranking.salvarDerrotado (filter ((/= Pl.playerId vencedor) . Pl.playerId) (players gs))
    
    --Inicia a proxima rodada
    else do
        --Determina de quem é a vez
        --Deve verificar se o proximo jogador é um bot exibindo um menu diferente
        putStrLn $ "\n Rodada: " ++ show (turnCount gs)
        let jogadorAtual = getCurrentPlayer gs
        putStrLn $ " Vez de: " ++ Pl.name jogadorAtual

        -- Oferece o menu de informações
        menuEntreTurnos jogadorAtual gs


        -- Executa o turno
        novoEstado <- playTurn gs

        -- Loop recursivo
        loopJogo novoEstado

menuEntreTurnos :: Pl.Player -> Board -> IO ()
menuEntreTurnos jogador gs = do
    putStrLn "\n Mais opções:"
    putStrLn "1. Continuar"
    putStrLn "2. Ver saldo"
    putStrLn "3. Ver propriedades"
    putStrLn "4. Ver posição atual"
    putStrLn "5. Ver rodada atual"
    putStrLn "6. Ver tudo"
    putStrLn "Escolha uma opção (ou ENTER para continuar): "
    opcao <- getLine
    case opcao of
        ""  -> return ()
        "1" -> return ()
        "2" -> do
            putStrLn $ " Saldo: R$" ++ show (Pl.balance jogador)
            menuEntreTurnos jogador gs
        "3" -> do
            putStrLn " Propriedades:"
            if null (Pl.properties jogador)
                then putStrLn "  (nenhuma)"
                else mapM_ (\p -> putStrLn $ " - " ++ Bh.houseName p) (Pl.properties jogador)
            menuEntreTurnos jogador gs
        "4" -> do
            putStrLn $ " Posição atual: " ++ show (Pl.position jogador)
            menuEntreTurnos jogador gs
        "5" -> do
            putStrLn $ " Rodada atual: " ++ show (turnCount gs)
            menuEntreTurnos jogador gs
        "6" -> do
            putStrLn $ " Saldo: R$" ++ show (Pl.balance jogador)
            putStrLn $ " Posição: " ++ show (Pl.position jogador)
            putStrLn $ " Rodada: " ++ show (turnCount gs)
            putStrLn " Propriedades:"
            if null (Pl.properties jogador)
                then putStrLn "  (nenhuma)"
                else mapM_ (\p -> putStrLn $ " - " ++ Bh.houseName p) (Pl.properties jogador)
            menuEntreTurnos jogador gs
        _   -> do
            putStrLn " Opção inválida."
            menuEntreTurnos jogador gs




