module Game.Interface where

--Guarda os prints do jogo

--aplyHouseEffect
printTaxHouseType :: String -> Int -> IO()
printTaxHouseType playerName tax = do putStrLn(playerName ++ " pagou R$" ++ (show tax) ++ " de imposto.")

printPrisonHouseType :: String -> IO()
printPrisonHouseType  playerName = putStrLn(playerName ++ " foi preso por 2 turnos!")

printEspecialHouseType :: String -> IO()
printEspecialHouseType playerName = putStrLn(playerName ++ "caiu em uma casa especial e vai jogar novamente!")

exceptionPrintNotFouldOwner :: IO()
exceptionPrintNotFouldOwner = putStrLn "Erro: proprietário não encontrado."

printRentPayment :: String -> Int -> String ->  IO()
printRentPayment payer rent receiver = putStrLn(payer ++ " pagou R$" ++ (show rent) ++ " de aluguel para " ++ receiver)

printPlayerWentBankrupt :: String -> IO()
printPlayerWentBankrupt playerName = putStrLn(playerName ++ " faliu!")

-- GameLoop

printPlayerBlocked :: String -> Int -> IO()
printPlayerBlocked playerName shifts = putStrLn(playerName ++ " está preso por " ++ (show shifts) ++ "turno(s)")

