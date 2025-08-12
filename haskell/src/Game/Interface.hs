module Game.Interface where

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
    putStr "\n"

    -- imprime laterais
    mapM_ putStrLn (mergeVertical left right)
    putStr "\n"

    -- imprime linha de baixo
    mapM_ putStrLn (mergeHorizontal bottom)


renderHouse :: BoardHouse -> [Player] -> [String]
renderHouse house ps =
    let pid   = printf "%02d" (houseId house)
        houseTitle = houseName house
        val   = if houseType house == "Cidade"
                   then "$" ++ show (fixedpurchaseValue house)
                   else ""
        shortName p = take 6 (name p)
        playerNames = map (\p -> "*" ++ shortName p ++ "*") ps
        -- máximo de linhas para jogadores
        playerLines = take 4 playerNames ++ replicate (max 0 (4 - length playerNames)) ""
    in [ "+-------------------+ "
       , "| " ++ centerText pid 17 ++ " | "
       , "|-------------------| "
       , "| " ++ centerText houseTitle 17 ++ " | "
       ] ++ map (\pl -> "| " ++ centerText pl 17 ++ " | ") playerLines
       ++ (if val /= "" then ["| " ++ centerText val 17 ++ " | "] else ["|                   | "])
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
mergeHorizontal = foldr1 (zipWith (++))

mergeVertical :: [[String]] -> [[String]] -> [String]
mergeVertical left right =
    let spaceBetween = replicate ((21 * 5) + 5) ' '
        leftLines    = concatVert left
        rightLines   = concatVert right
        diff         = 0
        paddedRight  = replicate diff (replicate (length (head rightLines)) ' ') ++ rightLines
    in zipWith (\l r -> l ++ spaceBetween ++ r) leftLines paddedRight

concatVert :: [[String]] -> [String]
concatVert = concat

--Prints de jogo

--aplyHouseEffect
printTaxHouseType :: String -> Int -> IO()
printTaxHouseType playerName tax = do putStrLn (playerName ++ " pagou R$" ++ show tax ++ " de imposto.")

printPrisonHouseType :: String -> IO()
printPrisonHouseType  playerName = putStrLn (playerName ++ " foi preso por 2 turnos!")

printEspecialHouseType :: String -> IO()
printEspecialHouseType playerName = putStrLn (playerName ++ " caiu em uma casa especial e vai jogar novamente!")

exceptionPrintNotFouldOwner :: IO()
exceptionPrintNotFouldOwner = putStrLn "Erro: proprietário não encontrado."

printRentPayment :: String -> Int -> String ->  IO()
printRentPayment payer rent receiver = putStrLn (payer ++ " pagou R$" ++ show rent ++ " de aluguel para " ++ receiver)

printPlayerWentBankrupt :: String -> IO()
printPlayerWentBankrupt playerName = putStrLn (playerName ++ " faliu!")

printHousePurchased :: String -> String -> IO()
printHousePurchased playerName purchasedHouseName = putStrLn (playerName ++ " comprou " ++ purchasedHouseName ++ "!")

printNoHaveMoney :: String -> IO()
printNoHaveMoney playerName = putStrLn (playerName ++ " não tem dinheiro suficiente.")

printSelfHouse :: String -> IO()
printSelfHouse playerName = putStrLn (playerName ++ " caiu na sua própria propriedade.")


printWantToAuction :: String -> IO()
printWantToAuction playerName = putStrLn (playerName ++ "Deseja fazer um leilão, vender imediatamente (s/leilao ou n/imediato)?")

printWantToSellHouse :: String -> IO()
printWantToSellHouse playerName  = putStrLn (playerName ++ "Deseja vender esta propriedade, construir uma nova casa ou continuar? (v/c/enter)")

printFreeHouse :: String -> IO()
printFreeHouse playerName = putStrLn (playerName ++ " encontrou uma cidade livre!")

printWantToBuyHouse :: String -> Int -> IO()
printWantToBuyHouse house housePrice = putStrLn ("Deseja comprar " ++ house ++ " por R$" ++ show housePrice ++ "? (s/n)")

printGetSalary :: String -> Int -> IO()
printGetSalary playerName salary = putStrLn (playerName ++ " Deu uma volta e recebeu R$"  ++ show salary ++ " de salário")

-- GameLoop

printMovedPlayer :: String -> Int -> Int -> IO()
printMovedPlayer playerName x y = putStrLn (playerName ++ " rolou " ++ show x ++ " e foi para a posição " ++ show y)

printAuctionBid :: String -> IO()
printAuctionBid playerName = putStrLn ("Qual seu lance jogador " ++ playerName ++ "?")

printSoldTo :: String -> IO()
printSoldTo playerName = putStrLn ("Vendido para: " ++ playerName)

printPlayerBlocked :: String -> Int -> IO()
printPlayerBlocked playerName shifts = putStrLn (playerName ++ " está preso por " ++ show shifts ++ "turno(s)")

printInsufficientBalanceHouse :: IO ()
printInsufficientBalanceHouse = putStrLn " Saldo insuficiente para construir uma casa."

printHouseBuiltSuccess :: IO ()
printHouseBuiltSuccess = putStrLn " Casa construída com sucesso!"

printInsufficientBalanceHotel :: IO ()
printInsufficientBalanceHotel = putStrLn " Saldo insuficiente para construir um hotel."

printHotelBuiltSuccess :: IO ()
printHotelBuiltSuccess = putStrLn " Hotel construído com sucesso!"

printMaxConstructionReached :: IO ()
printMaxConstructionReached = putStrLn " Esta propriedade já atingiu o limite máximo de construções."


printSellHouse :: String -> String -> IO()
printSellHouse playerName soldHouseName = putStrLn (playerName ++ " vendeu sua propriedade: " ++ soldHouseName)