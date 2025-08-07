module Game.GameLoop where

import qualified Game.BoardHouse as Bh
import System.Random (randomRIO)
import Game.Player
import Game.BoardHouse
import Game.Board
import Data.List (find)
import Game.Interface


--Falta mecanica de ganhar dinheiro quando da uma volta
--Decrementa os turnos bloqueados e retorna o novo board
blockedPlayer :: Player -> Board -> IO Board
blockedPlayer player gs = do 
    putStrLn((name player) ++ " está preso por " ++ show (blockedShifts player) ++ " turno(s).")
    let updatedPlayer = decrementBlockedShifts player
    return  $ nextPlayer $ updateCurrentPlayer gs updatedPlayer

movePlayer :: Player -> Board -> IO Board
movePlayer player gs = do
    dice <- rollDice
    let movedPlayer = advancePosition player dice (maxPosition gs)
    putStrLn $ name movedPlayer ++ " rolou " ++ show dice ++ " e foi para a posição " ++ show (position movedPlayer)

    case getBoardHouseById gs (position movedPlayer) of
        Nothing -> do
            putStrLn "Erro: posição inválida no tabuleiro."
            return $ nextPlayer $ updateCurrentPlayer gs movedPlayer
        --Chama função que aplica o efeito da casa
        Just house -> do
            gs1 <- applyHouseEffect gs movedPlayer house
            processConstructionIfAvailable gs1


canBuildOnHouse :: Player -> BoardHouse -> Bool
canBuildOnHouse player house =
    houseType house == "Cidade"
    && hasOwner house
    && any ((== houseId house) . Bh.houseId) (properties player)


processConstructionIfAvailable :: Board -> IO Board
processConstructionIfAvailable gs = 
    let player = getCurrentPlayer gs
        pos = position player
    in case getBoardHouseById gs pos of
        Just house | canBuildOnHouse player house -> do
            gs' <- construirUmaUnicaVez gs player house
            return $ nextPlayer gs'
        _ -> return $ nextPlayer gs



-- Função principal que roda o turno de um jogador
playTurn :: Board -> IO Board
playTurn gs = do
    let player = getCurrentPlayer gs
    --Verifica se o jogador esta preso
    if isBlocked player then do
        blockedPlayer player gs
        
    else do
        movePlayer player gs

-- Rola o dado (1 a 6)
rollDice :: IO Int
rollDice = randomRIO (1, 6)

    -- Aplica o efeito de acordo com o tipo da casa
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
        --codigo complexo; espaço para modularização
        if hasOwner house then do
            let ownerMaybe = find (\p -> any ((== houseId house) . Bh.houseId) (properties p)) (players gs)
            case ownerMaybe of
                Nothing -> do
                    exceptionPrintNotFouldOwner
                    return $ updateCurrentPlayer gs player
                --Mecanica de pagar o aluguel
                Just owner -> do
                    if playerId owner == playerId player then do
                        -- Jogador é o dono da casa
                        putStrLn $ name player ++ " caiu na própria propriedade."
                        putStrLn "Deseja vender esta propriedade? (s/n)"
                        resp <- getLine
                        if resp == "s"
                            then do
                                sellHouse player house gs
                            else
                                return $ updateCurrentPlayer gs player
                    else do
                        --Tranforma em função
                        let rent = rentalValue house
                        let payer = takeMoney player rent
                        let receiver = addMoney owner rent
                        printRentPayment (name payer) rent (name receiver)

                        --Jogador faliu e foi removido
                        if isBankrupt payer then do
                            printPlayerWentBankrupt $ name payer
                            let gs1 = removePlayer gs (playerId payer)
                            return gs1
                        else do

                            --atualizao estado do jogo
                            let gs1 = updatePlayerById gs payer
                            let gs2 = updatePlayerById gs1 receiver
                            return gs2

        --caso não tenha dono
        else do
            --Comprar ou não a cidade
            putStrLn $ name player ++ " encontrou uma cidade livre!"
            putStrLn $ "Deseja comprar " ++ houseName house ++ " por R$" ++ show (fixedpurchaseValue house) ++ "? (s/n)"
            response <- getLine
            --Mecanica de compra de cidade
            if response == "s" then
                buyHouseBoard player house gs
            else
                return $ updateCurrentPlayer gs player

    --ERRO:Casa sem efeito
    _ -> do
        putStrLn "Casa sem efeito definido."
        return $ updateCurrentPlayer gs player


sellHouse :: Player -> BoardHouse -> Board -> IO Board
sellHouse player house board = do
    let newPlayerBalance = addMoney player (fixedSalesValue house)
    let newPlayer = removePropertyById newPlayerBalance (houseId house)
    printSellHouse (name player) (houseName house)
    return $ updateCurrentPlayer board newPlayer
    

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

--Invalido
construirUmaUnicaVez :: Board -> Player -> Bh.BoardHouse -> IO Board
construirUmaUnicaVez gs player casa
    | Bh.numberCivilHouses casa < 2 = do
        let custo = Bh.fixedCivilHouseValue casa
        if balance player < custo then do
            putStrLn " Saldo insuficiente para construir uma casa."
            return gs
        else do
            let novaCasa = Bh.incrementNumberCivilHouses casa
            let novoPlayer = addProperty (removePropertyById (takeMoney player custo) (Bh.houseId casa)) novaCasa
            putStrLn " Casa construída com sucesso!"
            return $ updatePlayerById (updateBoardHouse gs novaCasa) novoPlayer

    | Bh.numberCivilHotels casa < 2 = do
        let custo = Bh.fixedCivilHotelValue casa
        if balance player < custo then do
            putStrLn " Saldo insuficiente para construir um hotel."
            return gs
        else do
            let novaCasa = Bh.incrementNumberCivilHotels casa
            let novoPlayer = addProperty (removePropertyById (takeMoney player custo) (Bh.houseId casa)) novaCasa
            putStrLn " Hotel construído com sucesso!"
            return $ updatePlayerById (updateBoardHouse gs novaCasa) novoPlayer

    | otherwise = do
        putStrLn " Esta propriedade já atingiu o limite máximo de construções."
        return gs


-- Imposto como 10% do saldo atual
calculateTax :: Player -> Int
calculateTax p = balance p `div` 10   


--------------------------------------------------------------------------------------

--playBot :: Player -> Board -> Board
--playBot bot board = do 
    --Calculo se a muitas casas de outros jogadores compradas a frente
    --Calculo se tenho dinheiro para resistir a passagem
    --Calculo quanto falta para dar um volta
    --Decido de forma aleatoria se compro ou não a casa


