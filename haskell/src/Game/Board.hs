module Game.Board where
    import Game.BoardHouse
    
    data Board = Board{

        title :: String,
        description :: String,
        nameOfInitialHouse :: String,
        idOfInitialHouse :: Int,
        openingBalance :: Int,
        balancePerShift :: Int,
        housesOnTheBoard :: [BoardHouse]

    } deriving(Show, Read)
