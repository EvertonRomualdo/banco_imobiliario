module Game.Player where
    import Game.BoardHouse

    data Player = Player{
        playerId :: Int,
        name :: String,
        position :: Int,
        balance :: Int,
        blockedShifts :: Int,
        properties :: [BoardHouse]
        
    } deriving (Show, Read)
