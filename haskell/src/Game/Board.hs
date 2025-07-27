module Game.Board where
    import qualified Game.BoardHouse as Bh
    
    data Board = Board{

        title :: String,
        description :: String,
        initialHouse :: Bh.BoardHouse,
        openingBalance :: Int,
        balancePerShift :: Int,
        housesOnTheBoard :: [Bh.BoardHouse]

    } deriving(Show, Read)
    

    
