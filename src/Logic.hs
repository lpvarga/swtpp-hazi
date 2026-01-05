module Logic where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char

import Board
import Data.Char()
import Data.Maybe()

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

buildMove :: String -> Maybe Move 
buildMove "" = Nothing
buildMove s = case break (=='-') s of 
  (a, '-':b) -> Just (Move (buildPos a) (buildPos b)) 
  _ -> error "Invalid move format"

-- helper functions and constants

data Direction = L | R | U | D | LU | RU | LD | RD deriving (Eq, Show)

pieceValue :: Cell -> Int
pieceValue Pawn  = 1
pieceValue Drone = 2
pieceValue Queen = 3
pieceValue Empty = 0

left, right, up, down, upLeft, upRight, downLeft, downRight :: Direction
left      = L
right     = R
up        = U
down      = D
upLeft    = LU
upRight   = RU
downLeft  = LD
downRight = RD 

directions :: [Direction]
directions = [left, up, right, down]

diagonalDirections :: [Direction]
diagonalDirections = [upLeft, upRight, downLeft, downRight]


minCol, maxCol :: Char
minCol = 'a'
maxCol = 'd'

minRow, maxRow :: Int
minRow = 0
maxRow = 7

isInsideBoard :: Pos -> Bool
isInsideBoard (Pos c r) =
  c >= minCol && c <= maxCol && r >= minRow && r <= maxRow

step :: Direction -> Pos -> Maybe Pos
step d (Pos c r)
  | d == left      && isInsideBoard (Pos (pred c) r)       = Just (Pos (pred c) r)
  | d == right     && isInsideBoard (Pos (succ c) r)       = Just (Pos (succ c) r)
  | d == up        && isInsideBoard (Pos c (r + 1))        = Just (Pos c (r + 1))
  | d == down      && isInsideBoard (Pos c (r - 1))        = Just (Pos c (r - 1))
  | d == upLeft    && isInsideBoard (Pos (pred c) (r + 1)) = Just (Pos (pred c) (r + 1))
  | d == upRight   && isInsideBoard (Pos (succ c) (r + 1)) = Just (Pos (succ c) (r + 1))
  | d == downLeft  && isInsideBoard (Pos (pred c) (r - 1)) = Just (Pos (pred c) (r - 1))
  | d == downRight && isInsideBoard (Pos (succ c) (r - 1)) = Just (Pos (succ c) (r - 1))
  | otherwise                                              = Nothing

toLeft, toRight, toUp, toDown :: Pos -> Maybe Pos
toLeft  = step left
toRight = step right
toUp    = step up
toDown  = step down

toUpLeft, toUpRight, toDownLeft, toDownRight :: Pos -> Maybe Pos
toUpLeft    = step upLeft
toUpRight   = step upRight
toDownLeft  = step downLeft
toDownRight = step downRight


posToIndices :: Pos -> (Int, Int)
posToIndices (Pos c r) = (7 - r, fromEnum c - fromEnum 'a')

--only needed in tests
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 v (_:xs) = v : xs
setAt n v (x:xs) = x : setAt (n - 1) v xs
-- Set a single cell on a board at a given Pos

--only needed in tests
setCell :: Board -> Pos -> Cell -> Board
setCell board p cell = setAt ri newRow board
  where
    (ri, ci) = posToIndices p
    oldRow   = board !! ri
    newRow   = setAt ci cell oldRow

whatIsInPosition :: Board -> Pos -> Maybe Cell
whatIsInPosition board pos
  | not (isInsideBoard pos) = Nothing
  | otherwise               = Just ((board !! ri) !! ci)
  where
    (ri, ci) = posToIndices pos

isTheFieldEmpty :: Board -> Pos -> Bool
isTheFieldEmpty board pos
  | whatIsInPosition board pos == Just Empty = True
  | otherwise                                = False

isCellInPosition :: Cell -> Board -> Pos -> Bool
isCellInPosition cell board pos =
  whatIsInPosition board pos == Just cell

isOnCurrentPlayerSide :: Player -> Pos -> Bool
isOnCurrentPlayerSide Top    (Pos _ r) = r >= 4 && r <= 7
isOnCurrentPlayerSide Bottom (Pos _ r) = r >= 0 && r <= 3

crossedCanal :: Pos -> Pos -> Bool
crossedCanal (Pos _ r1) (Pos _ r2) = topHalf r1 /= topHalf r2
  where
    topHalf :: Int -> Bool
    topHalf r = r >= 4


forbiddenTakebackTarget :: Pos -> Maybe Move -> Maybe Pos
forbiddenTakebackTarget _ Nothing = Nothing
forbiddenTakebackTarget pos (Just (Move s t))
  | crossedCanal s t && pos == t = Just s
  | otherwise                    = Nothing

filterForbiddenTarget :: Maybe Pos -> [Move] -> [Move]
filterForbiddenTarget Nothing ms = ms
filterForbiddenTarget (Just bad) ms =
  filter (\(Move _ trg) -> trg /= bad) ms


ray :: Pos -> Direction -> [Pos]
ray pos dir = go (step dir pos)
  where
    go :: Maybe Pos -> [Pos]
    go Nothing        = []
    go (Just nextPos) = nextPos : ray nextPos dir


applyRayRules :: Board -> Player -> [Pos] -> [Pos]
applyRayRules _ _ [] = []
applyRayRules board player (p:ps)
  | isCellInPosition Empty board p = p : applyRayRules board player ps
  | isOnCurrentPlayerSide player p = []
  | otherwise                      = [p]

targetsToMoves :: Pos -> [Pos] -> [Move]
targetsToMoves _ [] = []
targetsToMoves startPos (p:ps) = Move startPos p : targetsToMoves startPos ps


-- ########################################################################################################
-- ################## pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]        ##################
-- ################## - 5 Functional Points                                              ##################
-- ########################################################################################################

applyPawnRules :: [Pos] -> [Pos]
applyPawnRules []     = []
applyPawnRules (p:_)  = [p]

pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves board player pos lastMove
  | not (isCellInPosition Pawn board pos)  = []
  | not (isOnCurrentPlayerSide player pos) = []
  | otherwise                              = filterForbiddenTarget bad moves
  where
    bad :: Maybe Pos
    bad = forbiddenTakebackTarget pos lastMove

    moves :: [Move]
    moves = targetsToMoves pos (pawnTargets board player pos)

    pawnTargets :: Board -> Player -> Pos -> [Pos]
    pawnTargets board' player' pos' = go diagonalDirections
      where
        go :: [Direction] -> [Pos]
        go [] = []
        go (d:ds) =
          applyPawnRules (applyRayRules board' player' (ray pos' d)) ++ go ds


-- #######################################################################################################
-- ################## droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 5 Functional Points                                             ##################
-- #######################################################################################################


droneTargets :: Board -> Player -> Pos -> [Pos]
droneTargets board' player' pos' = go directions
  where
    go :: [Direction] -> [Pos]
    go [] = []
    go (d:ds) =
      applyDroneRules d (applyRayRules board' player' (ray pos' d)) ++ go ds

    applyDroneRules :: Direction -> [Pos] -> [Pos]
    applyDroneRules dir = take maxDist
      where
        maxDist :: Int
        maxDist = max 1 (countPiecesInDirection dir)

        countPiecesInDirection :: Direction -> Int
        countPiecesInDirection dir' = length (filter (isPieceAt dir') (ray pos' dir'))
          where
            isPieceAt :: Direction -> Pos -> Bool
            isPieceAt _ p = not (isCellInPosition Empty board' p)

droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves board player pos lastMove
  | not (isCellInPosition Drone board pos) = []
  | not (isOnCurrentPlayerSide player pos) = []
  | otherwise                              = filterForbiddenTarget bad moves
  where
    bad :: Maybe Pos
    bad = forbiddenTakebackTarget pos lastMove

    moves :: [Move]
    moves = targetsToMoves pos (droneTargets board player pos)


-- #######################################################################################################
-- ################## queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves board player pos lastMove
  | not (isCellInPosition Queen board pos) = []
  | not (isOnCurrentPlayerSide player pos) = []
  | otherwise                              = filterForbiddenTarget bad moves
  where
    bad :: Maybe Pos
    bad = forbiddenTakebackTarget pos lastMove

    moves :: [Move]
    moves = targetsToMoves pos (queenTargets board player pos)

    queenTargets :: Board -> Player -> Pos -> [Pos]
    queenTargets board' player' pos' = go (directions ++ diagonalDirections)
      where
        go :: [Direction] -> [Pos]
        go [] = []
        go (d:ds) =
          applyRayRules board' player' (ray pos' d) ++ go ds


-- #######################################################################################################
-- ################## makeMove :: Board -> Move -> (Board -> Int)                       ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

makeMove :: Board -> Move -> (Board, Int)
makeMove board (Move s t) = (boardAfter, score)
  where
    movingPiece :: Cell
    movingPiece = getCellOrEmpty (whatIsInPosition board s)

    capturedPiece :: Cell
    capturedPiece = getCellOrEmpty (whatIsInPosition board t)

    score :: Int
    score
      | capturedPiece == Empty = 0
      | otherwise              = pieceValue capturedPiece

    boardAfter :: Board
    boardAfter = setCell (setCell board s Empty) t movingPiece

    getCellOrEmpty :: Maybe Cell -> Cell
    getCellOrEmpty (Just c) = c
    getCellOrEmpty Nothing  = Empty


-- #######################################################################################################
-- ################## playerWon :: Board -> Player -> Int -> Int -> Maybe Player        ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon _ _ _ _ = Nothing
