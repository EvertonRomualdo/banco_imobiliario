import System.IO (hFlush, stdout)
import qualified Game.Player as Pl
import Game.Board
import  MyLib
import qualified Game.Ranking

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Banco Imobiliário Terminal!"
    homeMenu

-- Função auxiliar para ler entrada com prompt
prompt :: String -> IO String
prompt userText = do
    putStr userText
    hFlush stdout
    getLine

homeMenu :: IO ()
homeMenu = do
    putStrLn "\nMenu:"
    putStrLn "1. Cadastrar jogadores"
    putStrLn "2. Ver ranking"
    putStrLn "3. Sair"
    option <- prompt "Escolha uma opção: "
    case option of
        "1" -> do
            players <- registerPlayers
            startGame players
        "2" -> do
            Game.Ranking.showRanking  
            homeMenu 
        "3" -> putStrLn "Até logo!"
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            homeMenu

registerPlayers :: IO [Pl.Player]
registerPlayers = do
    qtyStr <- prompt "Quantos jogadores no total? (2 a 4): "
    case reads qtyStr of
        [(total, "")] | total >= 2 && total <= 4 -> do
            botsStr <- prompt $ "Quantos bots? (0 a " ++ show (total - 1) ++ "): "
            case reads botsStr of
                [(bots, "")] 
                    | bots >= 0 && bots <= total - 1 -> do
                        let finalBots = if total - bots == 1 && bots == 0 then 1 else bots
                        let humans = total - finalBots
                        humansList <- mapM createPlayer [1..humans]
                        let botIds = [(humans + 1)..total]
                        let botsList = createBots botIds
                        return (humansList ++ botsList)
                    | otherwise -> invalidInput
                _ -> invalidInput
        _ -> invalidInput
  where
    invalidInput = do
        putStrLn "Entrada inválida. Tente novamente."
        registerPlayers

createPlayer :: Int -> IO Pl.Player
createPlayer pid = do
    name <- prompt $ "Nome do jogador " ++ show pid ++ ": "
    return $ Pl.Player pid name 0 1000 0 [] False

createBots :: [Int] -> [Pl.Player]
createBots (a:as) = do
    let pl = createBot a 

    if as == [] then 
        [pl]
    else
        [pl] ++ createBots as

startGame :: [Pl.Player] -> IO ()
startGame players = do
    let board = tabuleiroInicial players
    MyLib.gameLoop board
