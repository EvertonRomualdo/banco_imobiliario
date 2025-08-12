module MyLib where

import Game.Ranking (saveWinner, saveLoser, showRanking)
import Game.GameLoop (playTurn)
import Game.Board
import Game.Interface (printBoard)

import qualified Game.Player as Pl
import qualified Game.BoardHouse as Bh

gameLoop :: Board -> IO ()
gameLoop gs = do

    -- Verifica se há um vencedor
    if length (players gs) <= 1 then do
        let winner = head (players gs)
        putStrLn $ "\n " ++ Pl.name winner ++ " venceu o jogo!"
        Game.Ranking.saveWinner winner
        mapM_ Game.Ranking.saveLoser (filter ((/= Pl.playerId winner) . Pl.playerId) (players gs))

    -- Inicia a próxima rodada
    else do
        -- Exibe tabuleiro antes do turno
        putStrLn "\n--- Tabuleiro ---"
        printBoard gs
        putStrLn "----------------------\n"
        -- Determina de quem é a vez
        putStrLn $ "\n Rodada: " ++ show (turnCount gs)
        let currentPlayer = getCurrentPlayer gs
        putStrLn $ " Vez de: " ++ Pl.name currentPlayer

        -- Oferece o menu de informações
        betweenTurnsMenu currentPlayer gs

        -- Executa o turno
        newState <- playTurn gs

        -- Loop recursivo
        gameLoop newState

createBot :: Int -> Pl.Player
createBot pid =
    Pl.Player
        { Pl.playerId = pid
        , Pl.name = botNames !! (pid `mod` length botNames)
        , Pl.position = 0
        , Pl.balance = 1000
        , Pl.blockedShifts = 0
        , Pl.properties = []
        , Pl.playerIsBot = True
        }

botNames :: [String]
botNames =
  [ "Bot Dinheirudo"
  , "Bot Quebrado Mas Feliz"
  , "Bot Investidor Misterioso"
  , "Bot Hipotecário"
  , "Bot Paga-Aluguel"
  , "Bot Passa-Go"
  , "Bot Compra-Tudo"
  , "Bot Corretor Fantasma"
  , "Bot Inquilino Sofrido"
  , "Bot Rei dos Dados"
  , "Bot Fazendeiro Imobiliário"
  , "Bot Dona Maria dos Aluguéis"
  , "Bot Esfomeado por Casas"
  , "Bot Vai à Falência"
  , "Bot Magnata do Bairro"
  , "Bot Comprador Compulsivo"
  , "Bot Prefeito de Tabuleirópolis"
  , "Bot Que Nunca Paga Imposto"
  , "Bot Sempre Preso"
  , "Bot Milionário Misterioso"
  ]

generateBotsFromIds :: [Int] -> [Pl.Player]
generateBotsFromIds ids = map createBot ids

betweenTurnsMenu :: Pl.Player -> Board -> IO ()
betweenTurnsMenu player gs = do
    putStrLn "\n Mais opções:"
    putStrLn "1. Continuar"
    putStrLn "2. Ver saldo"
    putStrLn "3. Ver propriedades"
    putStrLn "4. Ver posição atual"
    putStrLn "5. Ver rodada atual"
    putStrLn "6. Ver tudo"
    putStrLn "Escolha uma opção (ou ENTER para continuar): "
    option <- getLine
    case option of
        ""  -> return ()
        "1" -> return ()
        "2" -> do
            putStrLn $ " Saldo: R$" ++ show (Pl.balance player)
            betweenTurnsMenu player gs
        "3" -> do
            putStrLn " Propriedades:"
            if null (Pl.properties player)
                then putStrLn "  (nenhuma)"
                else mapM_ (\p -> putStrLn $ " - " ++ Bh.houseName p) (Pl.properties player)
            betweenTurnsMenu player gs
        "4" -> do
            putStrLn $ " Posição atual: " ++ show (Pl.position player)
            betweenTurnsMenu player gs
        "5" -> do
            putStrLn $ " Rodada atual: " ++ show (turnCount gs)
            betweenTurnsMenu player gs
        "6" -> do
            putStrLn $ " Saldo: R$" ++ show (Pl.balance player)
            putStrLn $ " Posição: " ++ show (Pl.position player)
            putStrLn $ " Rodada: " ++ show (turnCount gs)
            putStrLn " Propriedades:"
            if null (Pl.properties player)
                then putStrLn "  (nenhuma)"
                else mapM_ (\p -> putStrLn $ " - " ++ Bh.houseName p) (Pl.properties player)
            betweenTurnsMenu player gs
        _   -> do
            putStrLn " Opção inválida."
            betweenTurnsMenu player gs



