module Main where

import System.IO (hFlush, stdout)
import qualified MyLib (loopJogo)
import qualified Game.Player as Pl
import qualified Game.Board as Gb
import Game.GameState (GameState(..))


main :: IO ()
main = do
    putStrLn "🎲 Bem-vindo ao Banco Imobiliário Terminal!"
    menuInicial

menuInicial :: IO ()
menuInicial = do
    putStrLn "\n📋 Menu:"
    putStrLn "1. Cadastrar jogadores"
    putStrLn "2. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> do
            jogadores <- cadastrarJogadores
            iniciarJogo jogadores
        "2" -> putStrLn "👋 Até logo!"
        _   -> do
            putStrLn "❌ Opção inválida, tente novamente."
            menuInicial

cadastrarJogadores :: IO [Pl.Player]
cadastrarJogadores = do
    putStr "Quantos jogadores? (2 a 4): "
    hFlush stdout
    qtdStr <- getLine
    case reads qtdStr :: [(Int, String)] of
        [(qtd, "")] | qtd >= 2 && qtd <= 4 -> mapM criarJogador [1..qtd]
        _ -> do
            putStrLn "❗ Entrada inválida. Digite um número entre 2 e 4."
            cadastrarJogadores

criarJogador :: Int -> IO Pl.Player
criarJogador pid = do
    putStr $ "Nome do jogador " ++ show pid ++ ": "
    hFlush stdout
    nome <- getLine
    return $ Pl.Player pid nome 0 1000 0 []

iniciarJogo :: [Pl.Player] -> IO ()
iniciarJogo jogadores = do
    let board = Gb.tabuleiroInicial
    let casas = Gb.housesOnTheBoard board
    let estadoInicial = GameState jogadores casas 0 20 0
    MyLib.loopJogo estadoInicial
