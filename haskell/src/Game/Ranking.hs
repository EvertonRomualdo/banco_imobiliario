module Game.Ranking where

import Game.Player
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.Exception (evaluate)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.List ( sortBy, sortOn )
import Data.Ord  (Down(..), comparing)


strictReadFile :: FilePath -> IO String
strictReadFile fp = do
  exists <- doesFileExist fp
  if not exists
     then return ""
     else withFile fp ReadMode $ \h -> do
            c <- hGetContents h
            _ <- evaluate (length c)
            return c

type StatLine = (String, Int, Int, Int, Int)
-- (Name, Wins, Losses, Total Balance, Number of properties)

rankingDir  :: FilePath
rankingDir  = "data"
rankingFile :: FilePath
rankingFile = rankingDir ++ "/ranking.txt"

headerLine :: String
headerLine = "Nome | Vitorias | Derrotas | Saldo Total | Propriedades"

-- Save the winning player
saveWinner :: Player -> IO ()
saveWinner player = updateStats player True

-- Save the losing players
saveLoser :: Player -> IO ()
saveLoser player = updateStats player False

-- Update or insert statistics in ranking
updateStats :: Player -> Bool -> IO ()
updateStats p won = do
    createDirectoryIfMissing True rankingDir
    content <- strictReadFile rankingFile
    let linesList = lines content

        oldData =
          case linesList of
            (cab:rest) | cab == headerLine -> rest
            _                              -> linesList

        stats = map safeReadLine oldData
        newLine  = generateLine p won
        updated  = updateList newLine stats

        updatedSorted = sortOn (\(_, vit, _, saldo, props) -> (Down vit, Down saldo, Down props)) updated

    writeFile rankingFile (unlines (headerLine : map showLine updatedSorted))

-- Generate the new line with updated data
generateLine :: Player -> Bool -> StatLine
generateLine p won =
    let playerName = name p
        wins = if won then 1 else 0
        losses = if won then 0 else 1
        balanceValue = balance p
        props = length (properties p)
    in (playerName, wins, losses, balanceValue, props)

-- Update the existing line or insert a new one
updateList :: StatLine -> [StatLine] -> [StatLine]
updateList new [] = [new]
updateList new@(playerName, wins, losses, balanceValue, props) ((n, v, d, s, p) : xs)
  | playerName == n = (n, v + wins, d + losses, s + balanceValue, p + props) : xs
  | otherwise       = (n, v, d, s, p) : updateList new xs

-- Convert file line to tuple
safeReadLine :: String -> StatLine
safeReadLine line =
    case reads line of
    [(t, "")] -> t
    _         -> ("<corrompido>", 0, 0, 0, 0)

-- Convert tuple to String
showLine :: StatLine -> String
showLine = show

-- Show ranking in terminal
showRanking :: IO ()
showRanking = do
  exists <- doesFileExist rankingFile
  if not exists
    then putStrLn "Ranking ainda não existe."
    else do
      content <- strictReadFile rankingFile
      let linesList = lines content
          withoutHeader = case linesList of
                             (cab:rest) | "Nome" `elem` words cab -> rest
                             _                                   -> linesList
          stats = map safeReadLine withoutHeader
          sorted = sortBy (flip (comparing (\(_, vit, _, _, _) -> vit))) stats
      putStrLn "\nRANKING DE JOGADORES:"
      putStrLn "Jogador | Vitórias | Derrotas | Saldo Total | Propriedades"
      mapM_ printLine sorted
  putStrLn "\nPressione Enter para voltar ao menu..."
  _ <- getLine
  return ()
  where
    printLine (n, v, d, s, p) =
      putStrLn $ n ++ " | " ++ show v ++ " | " ++ show d ++ " | R$" ++ show s ++ " | " ++ show p
