import System.IO (hFlush, stdout)
import qualified Game.Player as Pl
import Game.Board
import  MyLib

------------------------------------------------------------------------------
--Codigo quase que completamente de GPT; incofiavel e complexo precisa de ajuste;
------------------------------------------------------------------------------


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
    putStrLn "2. Sair"
    opcao <- prompt "Escolha uma opção: "
    case opcao of
        "1" -> do
            jogadores <- cadastrarJogadores
            iniciarJogo jogadores
        "2" -> putStrLn "Até logo!"
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

-- Função para criar bots
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
