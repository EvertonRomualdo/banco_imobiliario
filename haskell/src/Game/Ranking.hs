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
-- (Nome, Vitórias, Derrotas, Saldo Total, Nº de propriedades)

rankingDir  :: FilePath
rankingDir  = "data"
rankingFile :: FilePath
rankingFile = rankingDir ++ "/ranking.txt"

headerLine :: String
headerLine = "Nome | Vitorias | Derrotas | Saldo Total | Propriedades"

-- Salva o jogador vencedor
salvarVencedor :: Player -> IO ()
salvarVencedor jogador = atualizarEstatisticas jogador True

-- Salva os jogadores derrotados
salvarDerrotado :: Player -> IO ()
salvarDerrotado jogador = atualizarEstatisticas jogador False

-- Atualiza ou insere estatísticas no ranking
atualizarEstatisticas :: Player -> Bool -> IO ()
atualizarEstatisticas p venceu = do
    createDirectoryIfMissing True rankingDir
    conteudo <- strictReadFile rankingFile
    let linhas = lines conteudo

        dadosAntigos =
          case linhas of
            (cab:rest) | cab == headerLine -> rest
            _                              -> linhas

        stats = map lerLinhaSegura dadosAntigos
        nova  = gerarLinha p venceu
        atual = atualizarLista nova stats

        atualOrdenado = sortOn (\(_, vit, _, saldo, props) -> (Down vit, Down saldo, Down props)) atual

    writeFile rankingFile (unlines (headerLine : map mostrarLinha atualOrdenado))

-- Gera a nova linha com os dados atualizados
gerarLinha :: Player -> Bool -> StatLine
gerarLinha p venceu =
    let nome = name p
        vit = if venceu then 1 else 0
        der = if venceu then 0 else 1
        saldo = balance p
        props = length (properties p)
    in (nome, vit, der, saldo, props)

-- Atualiza a linha existente ou insere nova
atualizarLista :: StatLine -> [StatLine] -> [StatLine]
atualizarLista nova [] = [nova]
atualizarLista nova@(nome, vit, der, saldo, props) ((n, v, d, s, p) : xs)
  | nome == n = (n, v + vit, d + der, s + saldo, p + props) : xs
  | otherwise = (n, v, d, s, p) : atualizarLista nova xs

-- Converte linha do arquivo para tupla
lerLinhaSegura :: String -> StatLine
lerLinhaSegura linha =
    case reads linha of
    [(t, "")] -> t
    _         -> ("<corrompido>", 0, 0, 0, 0)

-- Converte tupla para String
mostrarLinha :: StatLine -> String
mostrarLinha = show

-- Exibe ranking no terminal
mostrarRanking :: IO ()
mostrarRanking = do
  existe <- doesFileExist rankingFile
  if not existe
    then putStrLn "Ranking ainda não existe."
    else do
      conteudo <- strictReadFile rankingFile
      let linhas = lines conteudo
          semCab = case linhas of
                     (cab:rest) | "Nome" `elem` words cab -> rest
                     _                                   -> linhas
          stats = map lerLinhaSegura semCab
          ordenado = sortBy (flip (comparing (\(_, vit, _, _, _) -> vit))) stats
      putStrLn "\nRANKING DE JOGADORES:"
      putStrLn "Jogador | Vitórias | Derrotas | Saldo Total | Propriedades"
      mapM_ imprimir ordenado
  putStrLn "\nPressione Enter para voltar ao menu..."
  _ <- getLine
  return ()
  where
    imprimir (n, v, d, s, p) =
      putStrLn $ n ++ " | " ++ show v ++ " | " ++ show d ++ " | R$" ++ show s ++ " | " ++ show p