module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char (digitToInt,isDigit)
import Text.Read()
import Control.Applicative (Alternative(empty))

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Given data types may NOT be changed            ################
-- #############################################################################

data Player = Top | Bottom deriving (Show, Read)
data Cell = Empty | Queen | Drone | Pawn deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Top Top = True
  (==) Bottom Bottom = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) Pawn Pawn = True
  (==) Drone Drone = True
  (==) Queen Queen = True
  (==) _ _ = False
  
startingFEN :: String
startingFEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

startingBoard :: [[Cell]]
startingBoard = [
  [Queen, Queen, Drone, Empty],
  [Queen, Drone, Pawn,  Empty],
  [Drone, Pawn,  Pawn,  Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Pawn,  Pawn,  Drone],
  [Empty, Pawn,  Drone, Queen],
  [Empty, Drone, Queen, Queen]
  ]

buildPos :: String -> Pos 
buildPos (c:rStr) = Pos c (read rStr) 
buildPos _ = error "Invalid position format"

-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

validateFEN :: String -> Bool
validateFEN = validateFENHelper 8 4 False
  where
    validateFENHelper :: Int -> Int -> Bool -> String -> Bool
    validateFENHelper rows spaces letterInBlock [] = rows == 1 && notInternalField
      where
        notInternalField = spaces == 0 || spaces == 4

    validateFENHelper rows spaces _ input
      | rows < 1 = False
      | spaces < 0 = False

    validateFENHelper rows spaces letterInBlock (c:cs)
      | c == '/' = (previousWasSlash || letterInBlock)  && notInternalField && rows > 1 && validateFENHelper (rows - 1) 4 False cs
      | c == 'p' || c == 'd' ||  c == 'q' = validateFENHelper rows (spaces - 1) True cs
      | c >= '1' && c <= '3' = validateFENHelper rows (spaces - digitToInt c) letterInBlock cs
      where
        notInternalField = spaces == 0 || spaces == 4
        previousWasSlash = spaces == 4
    validateFENHelper _ _ _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard fen = map convertFenToRow (splitFEN fen)
  where
    charToCell :: Char -> Cell
    charToCell 'q' = Queen
    charToCell 'd' = Drone
    charToCell 'p' = Pawn
    charToCell 'e' = Empty

    foo :: String -> String
    -- foo [] = "eeee"                 had to remove, but was super ugly, would loved to leave in xddd
    foo [] = ""
    foo (c:cs)
      | isDigit c = replicate (digitToInt c) 'e' ++ foo cs
      | otherwise = c : foo cs

    normalizeRow :: String -> String
    normalizeRow s = take 4 (s ++ replicate 4 'e')

    convertFenToRow :: String -> [Cell]
    convertFenToRow fenrow =  map charToCell (normalizeRow (foo fenrow))

    splitFEN :: String -> [String]
    splitFEN string = splitFENHelper string []
      where
        splitFENHelper :: String -> String -> [String]
        splitFENHelper [] acc = [acc]
        splitFENHelper (c:cs) acc
          | c == '/' = acc : splitFENHelper cs []
          | otherwise = splitFENHelper cs (acc ++ [c])

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = compressFEN (buildFENHelper board)
  where
    buildFENHelper :: Board -> String
    buildFENHelper [row] = convertRow row
    buildFENHelper (row:rows) = convertRow row ++ "/" ++ buildFENHelper rows

    cellToChar :: Cell -> Char
    cellToChar Queen = 'q'
    cellToChar Drone = 'd'
    cellToChar Pawn = 'p'
    cellToChar Empty = 'e'

    convertRow :: [Cell] -> String
    convertRow = map cellToChar

    compressFEN :: String -> String
    compressFEN [] = []
    compressFEN ('e':'e':'e':'e':xs) = compressFEN xs
    compressFEN ('e':xs) =
      show n ++ compressFEN rest
      where
        n    = 1 + length (takeWhile (=='e') xs)
        rest = drop (n - 1) xs

    compressFEN (c:cs) = c : compressFEN cs