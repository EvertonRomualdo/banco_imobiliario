module Game.Board (
    Board(..),
    housesOnTheBoard,
    tabuleiroInicial,
    gerarCasas, 
    getCurrentPlayer,
    updateCurrentPlayer,
    updatePlayerById,
    nextPlayer,
    removePlayer,
    updateBoardHouse,
    getBoardHouseById
) where

import Game.Player
import Game.BoardHouse

data Board = Board {
    title :: String,
    description :: String,
    initialHouse :: BoardHouse,
    openingBalance :: Int,
    balancePerShift :: Int,
    housesOnTheBoard :: [BoardHouse],
    players :: [Player],           
    currentPlayerIndex :: Int,        
    maxPosition :: Int,               
    turnCount :: Int
} deriving (Show, Read)

-- Obtém o jogador atual
getCurrentPlayer :: Board -> Player
getCurrentPlayer board = players board !! currentPlayerIndex board

-- Atualiza um jogador na lista (substitui o jogador atual)
updateCurrentPlayer :: Board -> Player -> Board
updateCurrentPlayer board updatedPlayer =
    let index = currentPlayerIndex board
        updatedPlayers = take index (players board) ++ [updatedPlayer] ++ drop (index + 1) (players board)
    in board { players = updatedPlayers }

-- Atualiza um jogador por ID (útil ao atualizar fora do turno atual)
updatePlayerById :: Board -> Player -> Board
updatePlayerById board updatedPlayer =
    let updatedPlayers = map (\p -> if playerId p == playerId updatedPlayer then updatedPlayer else p) (players board)
    in board { players = updatedPlayers }

-- Avança para o próximo jogador
nextPlayer :: Board -> Board
nextPlayer board =
    let totalPlayers = length (players board)
        nextIdx = (currentPlayerIndex board + 1) `mod` totalPlayers -- melhorar
    in board { currentPlayerIndex = nextIdx, turnCount = turnCount board + 1 }

-- Remove jogador por ID (por falência, por exemplo)
removePlayer :: Board -> Int -> Board
removePlayer board pid =
    let updatedPlayers = filter (\p -> playerId p /= pid) (players board)
        newIdx = if currentPlayerIndex board >= length updatedPlayers then 0 else currentPlayerIndex board
    in board { players = updatedPlayers, currentPlayerIndex = newIdx }

-- Substitui uma casa no tabuleiro (por ID)
updateBoardHouse :: Board -> BoardHouse -> Board
updateBoardHouse board newHouse =
    let updatedBoard = map (\h -> if houseId h == houseId newHouse then newHouse else h) (housesOnTheBoard board)
    in board { housesOnTheBoard = updatedBoard }

getBoardHouseById :: Board -> Int -> Maybe BoardHouse
getBoardHouseById board hid = 
    case filter (\h -> houseId h == hid) (housesOnTheBoard board) of
        [] -> Nothing
        (h:_) -> Just h

gerarCasas :: [BoardHouse]
gerarCasas =
  [ BoardHouse 0  "Início"          "Especial" 0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 1  "Cidade Azul"     "Cidade"   300 500  50 100 20  40  0  0  50 50 False
  , BoardHouse 2  "Imposto Estadual""Imposto"  0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 3  "Cidade Verde"    "Cidade"   400 600  50 100 25  50  0  0  60 60 False
  , BoardHouse 4  "Prisao"          "Prisao"   0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 5  "Cidade Vermelha" "Cidade"   350 550  50 100 22  42  0  0  55 55 False
  , BoardHouse 6  "Imposto Federal" "Imposto"  0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 7  "Cidade Amarela"  "Cidade"   500 700  50 100 30  50  0  0  70 70 False
  , BoardHouse 8  "Especial Dado"   "Especial" 0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 9  "Cidade Laranja"  "Cidade"   450 650  50 100 28  45  0  0  65 65 False
  , BoardHouse 10 "Cidade Cinza"    "Cidade"   300 500  50 100 20  40  0  0  50 50 False
  , BoardHouse 11 "Cidade Preta"    "Cidade"   400 600  50 100 25  45  0  0  60 60 False
  , BoardHouse 12 "Cidade Branca"   "Cidade"   380 580  50 100 23  43  0  0  58 58 False
  , BoardHouse 13 "Prisao 2"        "Prisao"   0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 14 "Cidade Marrom"   "Cidade"   470 670  50 100 29  50  0  0  68 68 False
  , BoardHouse 15 "Especial Dado+"  "Especial" 0   0    0   0   0   0   0  0  0  0  False
  , BoardHouse 16 "Cidade Rosa"     "Cidade"   350 550  50 100 21  41  0  0  53 53 False
  , BoardHouse 17 "Cidade Roxa"     "Cidade"   360 560  50 100 22  42  0  0  54 54 False
  , BoardHouse 18 "Cidade Ciano"    "Cidade"   390 590  50 100 24  46  0  0  59 59 False
  , BoardHouse 19 "Cidade Dourada"  "Cidade"   550 800  50 100 35  55  0  0  80 80 False
  ]

tabuleiroInicial :: [Player] -> Board
tabuleiroInicial pls =
  let casas = gerarCasas
  in Board
      "Banco Imobiliário Terminal"
      "Versão de 20 casas, com imposto, prisão e especiais"
      (head casas)
      1000
      200
      casas
      pls
      0
      (length casas - 1)
      0