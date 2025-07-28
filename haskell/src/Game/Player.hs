module Game.Player where
    import qualified Game.BoardHouse as Bh

    data Player = Player{
        playerId :: Int,
        name :: String,
        position :: Int,
        balance :: Int,
        blockedShifts :: Int,
        properties :: [Bh.BoardHouse]
        
    } deriving (Show, Read)

    advancePosition :: Player -> Int -> Int -> Player
    advancePosition p value maxPosition = p{position = mod (position p + value) maxPosition}
    
    addMoney :: Player -> Int -> Player
    addMoney p  value = p { balance = balance p + value}

    takeMoney :: Player -> Int -> Player
    takeMoney p value = p { balance = balance p - value}

    setBlockedShifts :: Player -> Int -> Player
    setBlockedShifts p shifts = p {blockedShifts = shifts}

    decrementBlockedShifts :: Player -> Player
    decrementBlockedShifts p = p {blockedShifts = blockedShifts p - 1}

    addProperty :: Player -> Bh.BoardHouse -> Player
    addProperty p property = p {properties = properties p ++ [property]}

    removePropertyById :: Player -> Int -> Player
    removePropertyById p i  = let newProperty = recursiveRemovePropertyById (properties p) [] i
        in p {properties = newProperty}
    
    recursiveRemovePropertyById :: [Bh.BoardHouse] -> [Bh.BoardHouse]-> Int -> [Bh.BoardHouse]
    recursiveRemovePropertyById (a:as) l n = 
        if Bh.houseId a == n then
            as ++ l
        else
            recursiveRemovePropertyById as (l ++ [a]) n 
            

    isBlocked :: Player -> Bool
    isBlocked p = blockedShifts p > 0

    isBankrupt :: Player -> Bool
    isBankrupt p = balance p < 0


