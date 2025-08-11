module Game.GameLoop where

import qualified Game.BoardHouse as Bh
import System.Random (randomRIO)
import Game.Player
import Game.BoardHouse
import Game.Board
import Data.List ( find, group )
import Game.Interface
import qualified Game.Ranking


-- Função principal que roda o turno de um jogador
playTurn :: Board -> IO Board
playTurn gs = do
    let player = getCurrentPlayer gs
    --Verifica se o jogador esta preso
    if isBlocked player then do
        blockedPlayer player gs

    else do
        movePlayer player gs

--Move o jogador
movePlayer :: Player -> Board -> IO Board
movePlayer player gs = do
    dice <- rollDice
    let movedPlayer = advancePosition player dice (maxPosition gs)
    putStrLn $ name movedPlayer ++ " rolou " ++ show dice ++ " e foi para a posição " ++ show (position movedPlayer)
    movedPlayerWithSalary <- paySalary movedPlayer player (maxPosition gs) (balancePerShift gs)

    case getBoardHouseById gs (position movedPlayerWithSalary) of
        Nothing -> do
            putStrLn "Erro: posição inválida no tabuleiro."
            return $ nextPlayer $ updateCurrentPlayer gs movedPlayerWithSalary
        --Chama função que aplica o efeito da casa
        Just house -> do
            gs1 <- applyHouseEffect gs movedPlayerWithSalary house
            return $ nextPlayer gs1

--Desbloqueia o jogador
blockedPlayer :: Player -> Board -> IO Board
blockedPlayer player gs = do
    putStrLn (name player ++ " está preso por " ++ show (blockedShifts player) ++ " turno(s).")
    let updatedPlayer = decrementBlockedShifts player
    return  $ nextPlayer $ updateCurrentPlayer gs updatedPlayer


--Aplica o efeito de cada casa ao jogador
--E suas ações estando naquela posição
applyHouseEffect :: Board -> Player -> BoardHouse -> IO Board
applyHouseEffect gs player house = case houseType house of
    "Imposto" -> do
        let imposto = calculateTax player
        let updatedPlayer = takeMoney player imposto
        printTaxHouseType (name player) imposto
        return $ updateCurrentPlayer gs updatedPlayer

    "Prisao" -> do
        let updatedPlayer = setBlockedShifts player 2
        printPrisonHouseType $ name player
        return $ updateCurrentPlayer gs updatedPlayer

    "Especial" -> do
        printEspecialHouseType $ name player
        let gs' = updateCurrentPlayer gs player
        playTurn gs'

    "Cidade" -> do
        if hasOwner house then do
            let ownerMaybe = find (\p -> any ((== houseId house) . Bh.houseId) (properties p)) (players gs)
            case ownerMaybe of
                Nothing -> do
                    exceptionPrintNotFouldOwner
                    return $ updateCurrentPlayer gs player
                Just owner -> do
                    if playerId owner == playerId player then do
                        -- caiu na própria propriedade
                        putStrLn $ name player ++ " caiu na própria propriedade."
                        if playerIsBot player then do

                            if botSellHouse gs player then
                                if botAuctionOrImediate gs then
                                    houseAuction player gs
                                else
                                    sellHouse player house gs
                            else if botBuildCivilHouse gs player then do
                                processConstructionIfAvailable gs player
                            else
                                return $ updateCurrentPlayer gs player
                        else do

                            putStrLn "Deseja vender esta propriedade, construir uma nova casa ou continuar? (v/c/enter)"
                            resp <- getLine
                            case resp of
                                "v" -> do
                                    putStrLn "Deseja fazer um leilão, vender imediatamente (s/leilao ou n/imediato)?"
                                    resp2 <- getLine
                                    if resp2 == "s"
                                        then houseAuction player gs
                                        else sellHouse player house gs
                                "c" -> do
                                    processConstructionIfAvailable gs player
                                _   -> return $ updateCurrentPlayer gs player
                    else do
                        printRentPayment (name player) (rentalValue house) (name owner)
                        balanceTransfer player owner (rentalValue house) gs
        else do
            -- cidade livre
            putStrLn $ name player ++ " encontrou uma cidade livre!"
            let price = fixedpurchaseValue house
            if playerIsBot player then do
                if botBuyHouse gs player price then
                    buyHouseBoard player house gs
                else
                    return $ updateCurrentPlayer gs player
            else do
                putStrLn $ "Deseja comprar " ++ houseName house ++ " por R$" ++ show price ++ "? (s/n)"
                response <- getLine
                if response == "s"
                    then buyHouseBoard player house gs
                    else return $ updateCurrentPlayer gs player

    _ -> do
        putStrLn "Casa sem efeito definido."
        return $ updateCurrentPlayer gs player

--Paga salario ao jogador
paySalary :: Player -> Player -> Int -> Int -> IO Player
paySalary movedPlayer oldPlayer maxPosition salary = do
    let newPosition = position movedPlayer
        oldPosition = position oldPlayer
    if newPosition < oldPosition then do
        putStrLn (name movedPlayer ++ " Deu uma volta e recebeu R$"  ++ show salary ++ " de salário")
        return (addMoney movedPlayer salary)
    else
        return movedPlayer


--Verifica se o jogador pode construir na casa
processConstructionIfAvailable :: Board -> Player -> IO Board
processConstructionIfAvailable gs player =
    case getBoardHouseById gs (position player) of
        Just house | canBuildOnHouse player house -> buildCivilHouse gs player house
        _ -> return $ updateCurrentPlayer gs player



canBuildOnHouse :: Player -> BoardHouse -> Bool
canBuildOnHouse player house =
    houseType house == "Cidade"
    && hasOwner house
    && any ((== houseId house) . Bh.houseId) (properties player)





-- Rola o dado (1 a 6)
rollDice :: IO Int
rollDice = randomRIO (1, 6)

--Faz tranferencia de saldo entre 2 jogadores
balanceTransfer :: Player -> Player -> Int -> Board -> IO Board
balanceTransfer payer receiver value board = do
    let payer2    = takeMoney payer value
    let receiver2 = addMoney receiver value

    if isBankrupt payer2
      then do
        printPlayerWentBankrupt $ name payer2
        -- registra o derrotado no ranking imediatamente
        Game.Ranking.salvarDerrotado payer2
        -- remove o jogador do tabuleiro
        let gs1 = removePlayer board (playerId payer2)
        return gs1
      else do
        -- aplica o débito e o crédito normalmente
        let gs1 = updatePlayerById board payer2
        let gs2 = updatePlayerById gs1 receiver2
        return gs2

--Faz o leilão de uma casa
houseAuction :: Player -> Board -> IO Board
houseAuction player board = do
    let boardTemp = removePlayer board (playerId player)
    (winnerId, bidValue) <- recursiveAuction (players boardTemp)

    case find (\p -> playerId p == winnerId) (players board) of
        Nothing -> return board

        Just p1 -> do
            putStrLn ("Vendido para: " ++ name p1)
            balanceTransfer p1 player bidValue board



recursiveAuction :: [Player] -> IO (Int, Int) -- (id, value)
recursiveAuction [] = return (-1, -1)
recursiveAuction (a:as) = do
    value <- if playerIsBot a
                then return (auctionBid a (a:as))
                else do
                    putStrLn ("Qual seu lance jogador " ++ name a ++ "?")
                    valueStr <- getLine
                    return (read valueStr :: Int)

    let idx = playerId a
    (bestId, bestValue) <- recursiveAuction as

    if bestValue == -1 || value > bestValue
        then return (idx, value)
        else return (bestId, bestValue)

--Vende uma casa
sellHouse :: Player -> BoardHouse -> Board -> IO Board
sellHouse player house board = do
    let newPlayerBalance = addMoney player (fixedSalesValue house)
    let newPlayer = removePropertyById newPlayerBalance (houseId house)
    printSellHouse (name player) (houseName house)
    return $ updateCurrentPlayer board newPlayer

--Compra uma casa
buyHouseBoard :: Player -> BoardHouse -> Board -> IO Board
buyHouseBoard player house gs = do
    if balance player >= fixedpurchaseValue house then do
        let newPlayer = addProperty (takeMoney player (fixedpurchaseValue house)) house
        let newHouse = setHasOwner house
        printHousePurchased (name player) (houseName house)
        return $ updateBoardHouse (updatePlayerById gs newPlayer) newHouse
    else do
        printNoHaveMoney $ name player
        return $ updateCurrentPlayer gs player

--Constroi uma casa civil na propriedade do jogador
buildCivilHouse :: Board -> Player -> Bh.BoardHouse -> IO Board
buildCivilHouse board player casa
    | Bh.numberCivilHouses casa < 2 = do
        let cost = Bh.fixedCivilHouseValue casa
        if balance player < cost then do
            putStrLn " Saldo insuficiente para construir uma casa."
            return board
        else do
            let newHouse = Bh.incrementNumberCivilHouses casa
            let newPlayer = addProperty (removePropertyById (takeMoney player cost) (Bh.houseId casa)) newHouse
            putStrLn " Casa construída com sucesso!"
            return $ updatePlayerById (updateBoardHouse board newHouse) newPlayer

    | Bh.numberCivilHotels casa < 2 = do
        let cost = Bh.fixedCivilHotelValue casa
        if balance player < cost then do
            putStrLn " Saldo insuficiente para construir um hotel."
            return board
        else do
            let newHouse = Bh.incrementNumberCivilHotels casa
            let newPlayer = addProperty (removePropertyById (takeMoney player cost) (Bh.houseId casa)) newHouse
            putStrLn " Hotel construído com sucesso!"
            return $ updatePlayerById (updateBoardHouse board newHouse) newPlayer

    | otherwise = do
        putStrLn " Esta propriedade já atingiu o limite máximo de construções."
        return board


-- Imposto como 10% do saldo atual
calculateTax :: Player -> Int
calculateTax p = balance p `div` 10


--------------------------------------------------------------------------------------

--Raciocinio que determina a ação de compra uma casa do bot
botBuyHouse :: Board -> Player -> Int -> Bool
botBuyHouse board player price =
    let averageBalance = calculateAverageBalance (players board) (length (players board))
        balancePlayer  = balance player
    in  balancePlayer >= price
        && balancePlayer >= averageBalance
        && not (thereIsDangerZone (housesOnTheBoard board) player)

--Raciocinio que determina a ação de vender uma casa do bot
botSellHouse :: Board -> Player -> Bool
botSellHouse board player = do
    let averageBalance = calculateAverageBalance (players board) (length (players board))
        balancePlayer = balance player

    if (wellBelowTheAverageBalance averageBalance balancePlayer) && (moreThanHalfwayThere (housesOnTheBoard board) (position player)) then
        True

    else
        False

--Determina se o bot deve leiloar ou comprar uma casa imediatamente
botAuctionOrImediate :: Board -> Bool
botAuctionOrImediate board =
    calculateAverageBalance (players board) (length (players board)) > 650

--Raciocinio que determina a ação de construir uma casa civil do bot
botBuildCivilHouse :: Board -> Player ->  Bool
botBuildCivilHouse board player =
    calculateAverageBalance (players board) (length (players board)) >= balance player

--Calcula a media de saldo dos jogadores necessaria para as ações dos bots
calculateAverageBalance :: [Player] -> Int -> Int
calculateAverageBalance playerList size =
    sumBalance playerList `div` size

sumBalance :: [Player] -> Int
sumBalance [] = 0
sumBalance (a:as) = balance a + sumBalance as

--Determina se o bot está mais da metade do caminho em direção ao inicio
moreThanHalfwayThere :: [BoardHouse] -> Int -> Bool
moreThanHalfwayThere boardHouseList pos =
    (length boardHouseList - (pos + 1)) > (length boardHouseList `div` 2)

--Determina se o bot está bem abaixo da média
wellBelowTheAverageBalance :: Int -> Int -> Bool
wellBelowTheAverageBalance average bal =
    bal <= div average 2

--Calcula o lance que o bot dara em uma leilão a depender de outros calculos
auctionBid :: Player -> [Player] ->  Int
auctionBid player playerList = do
    let averageBalance = calculateAverageBalance playerList (length playerList)
        playerBalance = balance player
    if playerBalance > averageBalance then do
        div (averageBalance * 60) 100
    else
        calculateTax player


--Verifica se o caminho no raio de 6 casas da posição do bot é perigoso
--Com base na presença de propriedades adversárias
thereIsDangerZone :: [Bh.BoardHouse] -> Player -> Bool
thereIsDangerZone board player =
    let startPos   = position player
        boardSize  = length board

        ownedIds   = map Bh.houseId (properties player)

        nextHouses = [ board !! ((startPos + step) `mod` boardSize) | step <- [1..6] ]

        dangerList = map (\h -> Bh.hasOwner h && Bh.houseId h `notElem` ownedIds) nextHouses

    in  any (>= 4) (map length (filter (\g -> not (null g) && head g) (group dangerList)))

