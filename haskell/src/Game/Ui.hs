module Game.Ui where

    import Game.GameState
    import Game.Player
    import Game.Board
    import Game.BoardHouse
    import Data.List

    playersAt :: Int -> [Player] -> [Player]
    playersAt pos = filter (\p -> position p == pos)

    playersStr :: [Player] -> String
    playersStr [] = ""
    playersStr ps = intercalate "," $ map (take 3 . name) ps

    pad :: Int -> String -> String
    pad n s = take n (s ++ replicate n ' ')

    shortName :: String -> String
    shortName = pad 6 . take 6
    {-
    renderHouse :: Int -> BoardHouse -> [Player] -> [String]
    renderHouse idx h ps =
        [ " +------+  " ,
            "| " ++ pad 2 (show idx) ++ "     | ",
            "| " ++ shortName (houseType h) ++ " | ",
            "| " ++ shortName (playersStr ps) ++ " | ",
            " +------+  "
        ]
    -}
    renderHouse :: Int -> BoardHouse -> [Player] -> [String]
    renderHouse idx house playersHere =
        let
            linhaTop = "+---------------+ "
            linhaSeparadora = "|---------------| "
            numLinha = "|   " ++ center 11 (pad 2 (show idx)) ++ " | "
            nomeCasa = "| " ++ center 13 (houseTypeOrName house) ++ " | "
            jogadoresLinha = if null playersHere
                            then "|               | "
                            else "| " ++ center 13 (playersStr playersHere) ++ " | "
            valorLinha = "| " ++ center 13 (infoCasa house) ++ " | "
        in
            [ linhaTop
            , numLinha
            , linhaSeparadora
            , nomeCasa
            , jogadoresLinha
            , valorLinha
            , linhaTop
            ]

    renderHorizontalLine :: [(Int, BoardHouse, [Player])] -> String
    renderHorizontalLine casas =
        let blocos = map (\(i, h, ps) -> renderHouse i h ps) casas
            linhas = transpose blocos
        in unlines (map concat (transpose blocos))

    renderMiddleLines :: [(Int, BoardHouse, [Player])] -> [(Int, BoardHouse, [Player])] -> String
    renderMiddleLines left right =
        let leftBlocks = map (\(i,h,ps) -> renderHouse i h ps) left
            rightBlocks = map (\(i,h,ps) -> renderHouse i h ps) right
            linhas = zipWith (\l r -> head l ++ replicate 44 ' ' ++ head r ++ "\n" ++
                                        l !! 1 ++ replicate 44 ' ' ++ r !! 1 ++ "\n" ++
                                        l !! 2 ++ replicate 44 ' ' ++ r !! 2 ++ "\n" ++
                                        l !! 3 ++ replicate 44 ' ' ++ r !! 3 ++ "\n" ++
                                        l !! 4 ++ replicate 44 ' ' ++ r !! 4)
                            leftBlocks rightBlocks
        in unlines linhas

    renderBoard :: GameState -> String
    renderBoard gs =
        let b = board gs
            ps = players gs

            top    = [0..6]
            right  = [7..9]
            bottom = reverse [10..16]
            left   = reverse [17..19]

            get i = b !! i
            withPlayers i = (i, get i, playersAt i ps)

            topLine = map withPlayers top
            rightLine = map withPlayers right
            bottomLine = map withPlayers bottom
            leftLine = map withPlayers left

        in
            renderHorizontalLine topLine ++
            renderMiddleLines leftLine rightLine ++
            renderHorizontalLine bottomLine

    center :: Int -> String -> String
    center width s =
        let len = length s
            padding = width - len
            left = padding `div` 2
            right = padding - left
        in replicate left ' ' ++ s ++ replicate right ' '

    houseTypeOrName :: BoardHouse -> String
    houseTypeOrName h =
        case houseType h of
            "inicio" -> "Início"
            "feriado" -> "Feriado"
            "imposto" -> "Imposto"
            "rolar" -> "Rolar dado"
            "prisao" -> "Prisão"
            _ -> take 13 (houseName h)

    infoCasa :: BoardHouse -> String
    infoCasa h
        | houseType h == "inicio" = "+500"
        | houseType h == "cidade" = "$" ++ show (rentalValue h)
        | houseType h == "imposto" = "-200"
        | houseType h == "feriado" = "Descanso"
        | otherwise = ""