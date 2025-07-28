module Game.Ranking where

import Game.Player
import System.Directory (doesFileExist)
import System.IO
import Data.List
import Data.Maybe (fromMaybe)

type StatLine = (String, Int, Int, Int, Int) 
-- (Nome, Vitórias, Derrotas, Saldo Total, Nº de propriedades)

rankingFile :: FilePath
rankingFile = "ranking.txt"

-- Salva o jogador vencedor
salvarVencedor :: Player -> IO ()
salvarVencedor jogador = atualizarEstatisticas jogador True

-- Salva os jogadores derrotados
salvarDerrotado :: Player -> IO ()
salvarDerrotado jogador = atualizarEstatisticas jogador False

-- Atualiza ou insere estatísticas no ranking
atualizarEstatisticas :: Player -> Bool -> IO ()
atualizarEstatisticas p venceu = do
    existe <- doesFileExist rankingFile
    conteudo <- if existe then readFile rankingFile else return ""
    let stats = map lerLinha (lines conteudo)
    let nova = gerarLinha p venceu
    let atualizado = atualizarLista nova stats
    writeFile rankingFile (unlines (map mostrarLinha atualizado))

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
atualizarLista (nome, vit, der, saldo, props) [] = [(nome, vit, der, saldo, props)]
atualizarLista nova@(nome, vit, der, saldo, props) (x@(n, v, d, s, p):xs)
    | nome == n = (n, v + vit, d + der, s + saldo, p + props) : xs
    | otherwise = x : atualizarLista nova xs

-- Converte linha do arquivo para tupla
lerLinha :: String -> StatLine
lerLinha linha = read linha :: StatLine

-- Converte tupla para String
mostrarLinha :: StatLine -> String
mostrarLinha = show

-- Exibe ranking no terminal
mostrarRanking :: IO ()
mostrarRanking = do
    existe <- doesFileExist rankingFile
    if not existe then
        putStrLn "Ranking ainda não existe."
    else do
        conteudo <- readFile rankingFile
        let stats = map lerLinha (lines conteudo)
        putStrLn "\n🏆 RANKING DE JOGADORES:"
        putStrLn "Jogador | Vitórias | Derrotas | Saldo Total | Propriedades"
        mapM_ imprimir stats
  where
    imprimir (n, v, d, s, p) =
        putStrLn $ n ++ " | " ++ show v ++ " | " ++ show d ++ " | R$" ++ show s ++ " | " ++ show p
