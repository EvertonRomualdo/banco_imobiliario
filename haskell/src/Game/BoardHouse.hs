module Game.BoardHouse where

    data BoardHouse = BoardHouse{
        houseId :: Int,
        houseName :: String,
        houseType :: String,
        fixedSalesValue :: Int,
        fixedpurchaseValue :: Int,
        numberCivilHouses :: Int,
        numberCivilianHotels :: Int,
        rentalValue :: Int,
        hasOwner :: Bool

    } deriving(Show, Read)