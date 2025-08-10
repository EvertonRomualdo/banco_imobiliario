module Game.Interface where

import Game.Player
import Game.Board
import Game.BoardHouse
import Data.List

import Game.Player
import Game.Board
import Game.BoardHouse
import Text.Printf (printf)

-- Função principal do tabuleiro
printBoard :: Board -> IO ()
printBoard board = do
    let hs = housesOnTheBoard board
        playersList = players board

        -- pega a versão em linhas de cada casa
        getHouseLines i = renderHouse (hs !! i) (filter (\p -> position p == i) playersList)

        -- posições
        top    = map getHouseLines [0..6]
        right  = map getHouseLines [7..9]
        bottom = map getHouseLines (reverse [10..16])
        left   = map getHouseLines (reverse [17..19])

    -- imprime linha do topo
    mapM_ putStrLn (mergeHorizontal top)
    putStr("\n")

    -- imprime laterais
    mapM_ putStrLn (mergeVertical left right)
    putStr("\n")

    -- imprime linha de baixo
    mapM_ putStrLn (mergeHorizontal bottom)


renderHouse :: BoardHouse -> [Player] -> [String]
renderHouse house ps =
    let pid   = printf "%02d" (houseId house)
        title = houseName house
        val   = if houseType house == "Cidade"
                   then "$" ++ show (fixedpurchaseValue house)
                   else ""
        shortName p = take 6 (name p)
        playerNames = map (\p -> "*" ++ shortName p ++ "*") ps
        -- máximo de linhas para jogadores
        playerLines = take 2 playerNames ++ replicate (max 0 (2 - length playerNames)) ""
    in [ "+-------------------+ "
       , "| " ++ centerText pid 17 ++ " | "
       , "|-------------------| "
       , "| " ++ centerText title 17 ++ " | "
       ] ++ map (\pl -> "| " ++ centerText pl 17 ++ " | ") playerLines
       ++ (if val /= "" then ["| " ++ centerText val 17 ++ " | "] else [])
       ++ ["+-------------------+ "]

-- Funções auxiliares
centerText :: String -> Int -> String
centerText str width =
    let len = length str
        spaces = max 0 (width - len)
        left = spaces `div` 2
        right = spaces - left
    in replicate left ' ' ++ str ++ replicate right ' '

mergeHorizontal :: [[String]] -> [String]
mergeHorizontal houses = foldr1 (zipWith (++)) houses

mergeVertical :: [[String]] -> [[String]] -> [String]
mergeVertical left right =
    let spaceBetween = replicate (length (head left)) ' '
        leftLines    = concatVert left
        rightLines   = concatVert right
        diff         = length leftLines - length rightLines
        paddedRight  = replicate diff (replicate (length (head rightLines)) ' ') ++ rightLines
    in zipWith (\l r -> l ++ spaceBetween ++ r) leftLines paddedRight

concatVert :: [[String]] -> [String]
concatVert = concat

--aplyHouseEffect
printTaxHouseType :: String -> Int -> IO()
printTaxHouseType playerName tax = do putStrLn(playerName ++ " pagou R$" ++ (show tax) ++ " de imposto.")

printPrisonHouseType :: String -> IO()
printPrisonHouseType  playerName = putStrLn(playerName ++ " foi preso por 2 turnos!")

printEspecialHouseType :: String -> IO()
printEspecialHouseType playerName = putStrLn(playerName ++ " caiu em uma casa especial e vai jogar novamente!")

exceptionPrintNotFouldOwner :: IO()
exceptionPrintNotFouldOwner = putStrLn "Erro: proprietário não encontrado."

printRentPayment :: String -> Int -> String ->  IO()
printRentPayment payer rent receiver = putStrLn(payer ++ " pagou R$" ++ (show rent) ++ " de aluguel para " ++ receiver)

printPlayerWentBankrupt :: String -> IO()
printPlayerWentBankrupt playerName = putStrLn(playerName ++ " faliu!")

printHousePurchased :: String -> String -> IO()
printHousePurchased playerName houseName = putStrLn(playerName ++ " comprou " ++ houseName ++ "!")

printNoHaveMoney :: String -> IO()
printNoHaveMoney playerName = putStrLn(playerName ++ " não tem dinheiro suficiente.")

-- GameLoop

printPlayerBlocked :: String -> Int -> IO()
printPlayerBlocked playerName shifts = putStrLn(playerName ++ " está preso por " ++ (show shifts) ++ "turno(s)")

printSellHouse :: String -> String -> IO()
printSellHouse playerName houseName = putStrLn(playerName ++ " vendeu sua propriedade: " ++ houseName)