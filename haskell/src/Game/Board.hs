module Game.Board (
    Board(..),
    gerarCasas
) where

import Game.BoardHouse

data Board = Board {
    title :: String,
    description :: String,
    initialHouse :: BoardHouse,
    openingBalance :: Int,
    balancePerShift :: Int,
    housesOnTheBoard :: [BoardHouse]
} deriving (Show, Read)

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
