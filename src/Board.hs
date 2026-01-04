module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char (digitToInt)
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
validateFEN = validateFENHelper 8 4
  where
    -- rowsLeft: how many rows still to parse (starts at 8)
    -- spacesLeft: how many squares still free in the current row (starts at 4)
    validateFENHelper :: Int -> Int -> String -> Bool
    validateFENHelper rowsLeft spacesLeft input
      | rowsLeft < 1     = False
      | spacesLeft < 0   = False
      | null input       =
          rowsLeft == 1 && (spacesLeft == 0 || spacesLeft == 4)
      | otherwise =
          case input of
            ('/':rest) ->
              (spacesLeft == 0 || spacesLeft == 4)
              && rowsLeft > 1
              && validateFENHelper (rowsLeft - 1) 4 rest

            (c:rest)
              | c == 'p' || c == 'd' || c == 'q' ->
                  validateFENHelper rowsLeft (spacesLeft - 1) rest

              | c >= '1' && c <= '3' ->
                  validateFENHelper rowsLeft (spacesLeft - digitToInt c) rest

              | otherwise ->
                  False



-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard _ = startingBoard

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = startingFEN
