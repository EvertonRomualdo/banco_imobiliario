module Game.GameState where

import Game.Player
import Game.BoardHouse

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

    -- Atualiza o jogador atual na lista
    updateCurrentPlayer :: GameState -> Player -> GameState
    updateCurrentPlayer gs updatedPlayer =
        let idx = currentPlayerIndex gs
            updatedPlayers = take idx (players gs) ++ [updatedPlayer] ++ drop (idx + 1) (players gs)
        in gs { players = updatedPlayers }

    -- Passa para o próximo jogador
    nextPlayer :: GameState -> GameState
    nextPlayer gs =
        let nextIdx = (currentPlayerIndex gs + 1) `mod` length (players gs)
        in gs { currentPlayerIndex = nextIdx, turnCount = turnCount gs + 1 }

    -- Remove jogador (quando falido)
    removePlayer :: GameState -> Int -> GameState
    removePlayer gs playerIdToRemove =
        let updatedPlayers = filter ((/= playerIdToRemove) . playerId) (players gs)
            newIndex = if currentPlayerIndex gs >= length updatedPlayers then 0 else currentPlayerIndex gs
        in gs { players = updatedPlayers, currentPlayerIndex = newIndex }
