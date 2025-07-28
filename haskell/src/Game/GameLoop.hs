module Game.GameLoop where

import qualified Game.BoardHouse as Bh
import System.Random (randomRIO)
import Game.Player
import Game.BoardHouse
import Game.GameState
import Data.List (find)


-- Função principal que roda o turno de um jogador
playTurn :: GameState -> IO GameState
playTurn gs = do
    let player = getCurrentPlayer gs
    if isBlocked player then do
        putStrLn $ name player ++ " está preso por " ++ show (blockedShifts player) ++ " turno(s)."
        let updatedPlayer = decrementBlockedShifts player
        return $ nextPlayer $ updateCurrentPlayer gs updatedPlayer
    else do
        dice <- rollDice
        let movedPlayer = advancePosition player dice (maxPosition gs)
        putStrLn $ name movedPlayer ++ " rolou " ++ show dice ++ " e foi para a posição " ++ show (position movedPlayer)
        case getBoardHouseById gs (position movedPlayer) of
            Nothing -> do
                putStrLn "Erro: posição inválida no tabuleiro."
                return $ nextPlayer $ updateCurrentPlayer gs movedPlayer
            Just house -> do
                gs1 <- applyHouseEffect gs movedPlayer house

                -- Após aplicar efeito, verificar se o jogador está parado em propriedade própria para construir
                let jogadorAtualizado = getCurrentPlayer gs1
                let pos = position jogadorAtualizado
                case getBoardHouseById gs1 pos of
                  Just casaAtual
                    | houseType casaAtual == "Cidade" &&
                      hasOwner casaAtual &&
                      any ((== houseId casaAtual) . Bh.houseId) (properties jogadorAtualizado) -> do
                          gs2 <- construirUmaUnicaVez gs1 jogadorAtualizado casaAtual
                          return $ nextPlayer gs2
                  _ -> return $ nextPlayer gs1

-- Rola o dado (1 a 6)
rollDice :: IO Int
rollDice = randomRIO (1, 6)

    -- Aplica o efeito de acordo com o tipo da casa
applyHouseEffect :: GameState -> Player -> BoardHouse -> IO GameState
applyHouseEffect gs player house = case houseType house of
    "Imposto" -> do
        let imposto = calculateTax player
        let updatedPlayer = takeMoney player imposto
        putStrLn $ name player ++ " pagou R$" ++ show imposto ++ " de imposto."
        return $ updateCurrentPlayer gs updatedPlayer

    "Prisao" -> do
        let updatedPlayer = setBlockedShifts player 2
        putStrLn $ name player ++ " foi preso por 2 turnos!"
        return $ updateCurrentPlayer gs updatedPlayer

    "Especial" -> do
        putStrLn $ name player ++ " caiu em uma casa especial e vai jogar novamente!"
        let gs' = updateCurrentPlayer gs player
        playTurn gs'

    "Cidade" -> do
        if hasOwner house then do
            let ownerMaybe = find (\p -> any ((== houseId house) . Bh.houseId) (properties p)) (players gs)
            case ownerMaybe of
                Nothing -> do
                    putStrLn "Erro: proprietário não encontrado."
                    return $ updateCurrentPlayer gs player
                Just owner -> do
                    let rent = rentalValue house
                    let payer = takeMoney player rent
                    let receiver = addMoney owner rent
                    putStrLn $ name payer ++ " pagou R$" ++ show rent ++ " de aluguel para " ++ name receiver

                    if isBankrupt payer then do
                        putStrLn $ name payer ++ " faliu!"
                        let gs1 = removePlayer gs (playerId payer)
                        return gs1
                    else do
                        let gs1 = updatePlayerById gs payer
                        let gs2 = updatePlayerById gs1 receiver
                        return gs2
        else do
            putStrLn $ name player ++ " encontrou uma cidade livre!"
            putStrLn $ "Deseja comprar " ++ houseName house ++ " por R$" ++ show (fixedpurchaseValue house) ++ "? (s/n)"
            response <- getLine
            if response == "s" then
                if balance player >= fixedpurchaseValue house then do
                    let newPlayer = addProperty (takeMoney player (fixedpurchaseValue house)) house
                    let newHouse = setHasOwner house
                    putStrLn $ name player ++ " comprou " ++ houseName house ++ "!"
                    return $ updateBoardHouse (updatePlayerById gs newPlayer) newHouse
                else do
                    putStrLn $ name player ++ " não tem dinheiro suficiente."
                    return $ updateCurrentPlayer gs player
            else
                return $ updateCurrentPlayer gs player

    _ -> do
        putStrLn "Casa sem efeito definido."
        return $ updateCurrentPlayer gs player

construirUmaUnicaVez :: GameState -> Player -> Bh.BoardHouse -> IO GameState
construirUmaUnicaVez gs player casa
    | Bh.numberCivilHouses casa < 2 = do
        let custo = Bh.fixedCivilHouseValue casa
        if balance player < custo then do
            putStrLn "💸 Saldo insuficiente para construir uma casa."
            return gs
        else do
            let novaCasa = Bh.incrementNumberCivilHouses casa
            let novoPlayer = addProperty (removePropertyById (takeMoney player custo) (Bh.houseId casa)) novaCasa
            putStrLn "🏠 Casa construída com sucesso!"
            return $ updatePlayerById (updateBoardHouse gs novaCasa) novoPlayer

    | Bh.numberCivilHotels casa < 2 = do
        let custo = Bh.fixedCivilHotelValue casa
        if balance player < custo then do
            putStrLn "💸 Saldo insuficiente para construir um hotel."
            return gs
        else do
            let novaCasa = Bh.incrementNumberCivilHotels casa
            let novoPlayer = addProperty (removePropertyById (takeMoney player custo) (Bh.houseId casa)) novaCasa
            putStrLn "🏨 Hotel construído com sucesso!"
            return $ updatePlayerById (updateBoardHouse gs novaCasa) novoPlayer

    | otherwise = do
        putStrLn "✅ Esta propriedade já atingiu o limite máximo de construções."
        return gs


-- Imposto como 10% do saldo atual
calculateTax :: Player -> Int
calculateTax p = balance p `div` 10   
