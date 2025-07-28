module Game.GameState where

import Game.Player
import Game.BoardHouse
import Game.Board   

data GameState = GameState {
    players :: [Player],               
    board :: [BoardHouse],             
    currentPlayerIndex :: Int,         
    maxPosition :: Int,               
    turnCount :: Int                   
} deriving (Show, Read)

-- Obtém o jogador atual
getCurrentPlayer :: GameState -> Player
getCurrentPlayer gs = players gs !! currentPlayerIndex gs

-- Atualiza um jogador na lista (substitui o jogador atual)
updateCurrentPlayer :: GameState -> Player -> GameState
updateCurrentPlayer gs updatedPlayer =
    let idx = currentPlayerIndex gs
        updatedPlayers = take idx (players gs) ++ [updatedPlayer] ++ drop (idx + 1) (players gs)
    in gs { players = updatedPlayers }

-- Atualiza um jogador por ID (útil ao atualizar fora do turno atual)
updatePlayerById :: GameState -> Player -> GameState
updatePlayerById gs updatedPlayer =
    let updatedPlayers = map (\p -> if playerId p == playerId updatedPlayer then updatedPlayer else p) (players gs)
    in gs { players = updatedPlayers }

-- Avança para o próximo jogador
nextPlayer :: GameState -> GameState
nextPlayer gs =
    let totalPlayers = length (players gs)
        nextIdx = (currentPlayerIndex gs + 1) `mod` totalPlayers
    in gs { currentPlayerIndex = nextIdx, turnCount = turnCount gs + 1 }

-- Remove jogador por ID (por falência, por exemplo)
removePlayer :: GameState -> Int -> GameState
removePlayer gs pid =
    let updatedPlayers = filter (\p -> playerId p /= pid) (players gs)
        newIdx = if currentPlayerIndex gs >= length updatedPlayers then 0 else currentPlayerIndex gs
    in gs { players = updatedPlayers, currentPlayerIndex = newIdx }

-- Substitui uma casa no tabuleiro (por ID)
updateBoardHouse :: GameState -> BoardHouse -> GameState
updateBoardHouse gs newHouse =
    let updatedBoard = map (\h -> if houseId h == houseId newHouse then newHouse else h) (board gs)
    in gs { board = updatedBoard }

-- Busca uma casa no tabuleiro por ID
getBoardHouseById :: GameState -> Int -> Maybe BoardHouse
getBoardHouseById gs hid = 
    case filter (\h -> houseId h == hid) (board gs) of
        [] -> Nothing
        (h:_) -> Just h
