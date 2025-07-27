module Game.BoardHouse where
    data BoardHouse = BoardHouse{
        houseId :: Int,
        houseName :: String,
        houseType :: String,
        fixedSalesValue :: Int,
        fixedpurchaseValue :: Int,
        fixedCivilHouseValue :: Int,
        fixedCivilHotelValue :: Int,
        fixedIncreaseInRentPerCivilHousing :: Int,
        fixedIncreaseInRentPerCivilHotel :: Int,
        numberCivilHouses :: Int,
        numberCivilHotels :: Int,
        fixedInitialRentalValue :: Int,
        rentalValue :: Int,
        hasOwner :: Bool

    } deriving(Show, Read)
    
    instance Eq BoardHouse where
        (==) h1 h2 = (houseId h1) == (houseId h2)


    updateRentalValue :: BoardHouse -> Int -> BoardHouse
    updateRentalValue board value = board{rentalValue = rentalValue board + value}

    incrementNumberCivilHouses :: BoardHouse -> BoardHouse
    incrementNumberCivilHouses board = 
        if numberCivilHouses board < 2 then 
            let newBoard = board{numberCivilHouses = numberCivilHouses board + 1}
            in updateRentalValue newBoard (fixedIncreaseInRentPerCivilHousing newBoard)
        else
            board 
    
    incrementNumberCivilHotels :: BoardHouse -> BoardHouse
    incrementNumberCivilHotels board = 
        if numberCivilHotels board < 2 then 
            let newBoard = board{numberCivilHotels = numberCivilHotels board + 1}
            in updateRentalValue newBoard (fixedIncreaseInRentPerCivilHotel newBoard)
        else
            board
    
    setHasOwner :: BoardHouse -> BoardHouse
    setHasOwner board = board{hasOwner = not (hasOwner board)}

    resetBoardHouse :: BoardHouse -> BoardHouse
    resetBoardHouse board = board{numberCivilHouses = 0, numberCivilHotels = 0, rentalValue = fixedInitialRentalValue board , hasOwner = False}

    

    


    