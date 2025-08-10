import System.IO (hFlush, stdout)
import qualified Game.Player as Pl
<<<<<<< HEAD
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
=======
import Game.Board
import  MyLib
import qualified Game.Ranking

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Banco Imobiliário Terminal!"
    menuInicial

-- Função auxiliar para ler entrada com prompt
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine

menuInicial :: IO ()
menuInicial = do
    putStrLn "\nMenu:"
    putStrLn "1. Cadastrar jogadores"
    putStrLn "2. Ver ranking"
    putStrLn "3. Sair"
    opcao <- prompt "Escolha uma opção: "
    case opcao of
        "1" -> do
            jogadores <- cadastrarJogadores
            iniciarJogo jogadores
        "2" -> do
            Game.Ranking.mostrarRanking  
            menuInicial 
        "3" -> putStrLn "Até logo!"
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            menuInicial

cadastrarJogadores :: IO [Pl.Player]
cadastrarJogadores = do
    qtdStr <- prompt "Quantos jogadores no total? (2 a 4): "
    case reads qtdStr of
        [(total, "")] | total >= 2 && total <= 4 -> do
            botsStr <- prompt $ "Quantos bots? (0 a " ++ show (total - 1) ++ "): "
            case reads botsStr of
                [(bots, "")] 
                    | bots >= 0 && bots <= total - 1 -> do
                        let botsFinal = if total - bots == 1 && bots == 0 then 1 else bots
                        let humanos = total - botsFinal
                        humanosList <- mapM criarJogador [1..humanos]
                        let botIds = [(humanos + 1)..total]
                        let botsList = criarBots botIds
                        return (humanosList ++ botsList)
                    | otherwise -> entradaInvalida
                _ -> entradaInvalida
        _ -> entradaInvalida
  where
    entradaInvalida = do
        putStrLn "Entrada inválida. Tente novamente."
        cadastrarJogadores

criarJogador :: Int -> IO Pl.Player
criarJogador pid = do
    nome <- prompt $ "Nome do jogador " ++ show pid ++ ": "
    return $ Pl.Player pid nome 0 1000 0 [] False


criarBots :: [Int] -> [Pl.Player]
criarBots (a:as) = do
    let pl = createBot a 

    if as == [] then 
        [pl]
    else
        [pl] ++ criarBots as


iniciarJogo :: [Pl.Player] -> IO ()
iniciarJogo jogadores = do
    let board = tabuleiroInicial jogadores
    MyLib.loopJogo board
>>>>>>> main
