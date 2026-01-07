-- #############################################################################
-- ###### TESTS                                                       ##########
-- #############################################################################

import Test.Hspec

import Board ( validateFEN, buildBoard, buildFEN
             , Player(Top, Bottom)
             , Cell(Empty, Pawn, Drone, Queen)
             , Pos(Pos)
             , startingBoard
             , Board
             )

import Logic (Move(Move), Direction, left, right, up, down, directions
  , diagonalDirections
  , setCell
  , isInsideBoard
  , toLeft, toRight, toUp, toDown
  , whatIsInPosition
  , isCellInPosition
  , isOnCurrentPlayerSide
  , crossedCanal, forbiddenTakebackTarget, filterForbiddenTarget
  , ray, applyRayRules, targetsToMoves
  , droneTargets
  , pieceValue
  , step
  , toUpLeft, toUpRight, toDownLeft, toDownRight
  , isTheFieldEmpty
  , isFigurePresentOnPlayerSide
  , setAt
  , makeMove
  , playerWon
  , setCell  
  , pawnMoves, droneMoves, queenMoves, makeMove, playerWon)


main :: IO ()
main = hspec $ do
  describe "Martian Chess - Board notation (FEN)" $ do
    ---------------------------------------------------------------------------
    -- validateFEN tests
    ---------------------------------------------------------------------------

    describe "validateFEN" $ do
      it "accepts 8 empty rows" $ do
        validateFEN "///////" `shouldBe` True

      it "rejects 9 rows" $ do
        validateFEN "////////" `shouldBe` False

      it "rejects 7 rows" $ do
        validateFEN "//////" `shouldBe` False

      it "accepts the example starting FEN from the sheet" $ do
        validateFEN "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` True

      it "rejects invalid digit 4" $ do
        validateFEN "qqd4///////" `shouldBe` False

      it "rejects invalid characters" $ do
        validateFEN "qqx1/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` False

      it "rejects row that is too short (not 4 squares)" $ do
        validateFEN "qq///////" `shouldBe` False

      it "rejects row that is too long (>4 squares)" $ do
        validateFEN "qqqqq///////" `shouldBe` False

      it "accepts empty first row" $ do
        validateFEN "/qqd1/qdp1/dpp1///1ppd/1pdq" `shouldBe` True

      it "accepts empty last row" $ do
        validateFEN "qqd1/qdp1/dpp1///1ppd/1pdq/" `shouldBe` True

      it "rejects slash inside an incomplete row (width 1..3)" $ do
        validateFEN "q/qdp1/dpp1///1ppd/1pdq/1dqq" `shouldBe` False
      
      it "rejects invalid expanded empty row example" $ do
        validateFEN "q21/qdd1/1p11/12q/pp11/pppd/13/d1q1" `shouldBe`False
    
    ---------------------------------------------------------------------------
    -- buildBoard tests
    ---------------------------------------------------------------------------
    describe "Unit-Board: buildBoard" $ do

      let fenExampleCanonical = "q3/qdd1/1p2/3q/pp2/pppd//d1q1"
      -- Same position, but with redundant digit splits (still valid per PDF)
      let fenExampleRedundant = "q21/qdd1/1p11/12q/pp11/pppd//d1q1"

      let expectedExampleBoard =
            [ [Queen, Empty, Empty, Empty]     -- row 7: "q3"
            , [Queen, Drone, Drone, Empty]     -- row 6: "qdd1"
            , [Empty, Pawn, Empty, Empty]      -- row 5: "1p2"
            , [Empty, Empty, Empty, Queen]     -- row 4: "3q"
            , [Pawn, Pawn, Empty, Empty]       -- row 3: "pp2"
            , [Pawn, Pawn, Pawn, Drone]        -- row 2: "pppd"
            , [Empty, Empty, Empty, Empty]     -- row 1: "" (empty row)
            , [Drone, Empty, Queen, Empty]     -- row 0: "d1q1"
            ]

      it "builds the PDF example board (canonical FEN)" $ do
        buildBoard fenExampleCanonical `shouldBe` expectedExampleBoard

      it "builds the same board from redundant-but-valid digit encoding" $ do
        buildBoard fenExampleRedundant `shouldBe` expectedExampleBoard

      it "maps indices correctly: (board!!0)!!0 is a7 and (board!!0)!!3 is d7" $ do
        let b = buildBoard fenExampleCanonical
        (b !! 0) !! 0 `shouldBe` Queen  -- a7
        (b !! 0) !! 3 `shouldBe` Empty  -- d7

      it "keeps empty rows as all Empty cells in the Board representation" $ do
        let b = buildBoard fenExampleCanonical
        b !! 6 `shouldBe` replicate 4 Empty  -- row 1 is empty in the FEN ("//")

    ---------------------------------------------------------------------------
    -- buildFEN tests
    ---------------------------------------------------------------------------
    describe "Unit-Board: buildFEN" $ do

      let fenExampleCanonical = "q3/qdd1/1p2/3q/pp2/pppd//d1q1"
      let fenExampleRedundant = "q21/qdd1/1p11/12q/pp11/pppd//d1q1"

      it "serializes the PDF example board back to the canonical FEN form" $ do
        buildFEN (buildBoard fenExampleCanonical) `shouldBe` fenExampleCanonical

      it "normalizes redundant digit encodings to the canonical compressed form" $ do
        -- Even if input uses q21 etc., buildFEN should output canonical (q3 etc.)
        buildFEN (buildBoard fenExampleRedundant) `shouldBe` fenExampleCanonical

      it "preserves an empty row as an empty segment between slashes" $ do
        -- The example has an empty row before the last row: .../pppd//d1q1
        buildFEN (buildBoard fenExampleCanonical) `shouldBe` fenExampleCanonical
        -- This assertion specifically checks that the double slash is there:
        let out = buildFEN (buildBoard fenExampleCanonical)
        out `shouldContain` "//"
    
    describe "buildBoard/buildFEN consistency" $ do
      it "buildFEN . buildBoard is identity for a valid FEN" $ do
        let fen = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
        buildFEN (buildBoard fen) `shouldBe` fen

      it "roundtrip works for empty board" $ do
        let fen = "///////"
        buildFEN (buildBoard fen) `shouldBe` fen
      
    describe "Martian Chess - Helper functions" $ do

    ---------------------------------------------------------------------------
    -- Direction constants / list
    ---------------------------------------------------------------------------
      describe "directions" $ do
        it "contains left, up, right, down in this order" $ do
          directions `shouldBe` [left, up, right, down]

      ---------------------------------------------------------------------------
      -- isInsideBoard
      ---------------------------------------------------------------------------
      describe "isInsideBoard" $ do
        it "returns True for corners and a middle field" $ do
          isInsideBoard (Pos 'a' 0) `shouldBe` True
          isInsideBoard (Pos 'd' 7) `shouldBe` True
          isInsideBoard (Pos 'c' 4) `shouldBe` True

        it "returns False for positions outside the 4x8 board" $ do
          isInsideBoard (Pos 'e' 0) `shouldBe` False
          isInsideBoard (Pos 'a' 8) `shouldBe` False
          isInsideBoard (Pos '`' 3) `shouldBe` False
          isInsideBoard (Pos 'b' (-1)) `shouldBe` False

      ---------------------------------------------------------------------------
      -- toLeft / toRight / toUp / toDown
      ---------------------------------------------------------------------------
      describe "step helpers" $ do
        it "toLeft returns Nothing at the left edge" $ do
          toLeft (Pos 'a' 3) `shouldBe` Nothing

        it "toLeft returns the neighbor to the left" $ do
          toLeft (Pos 'b' 3) `shouldBe` Just (Pos 'a' 3)

        it "toRight returns Nothing at the right edge" $ do
          toRight (Pos 'd' 3) `shouldBe` Nothing

        it "toRight returns the neighbor to the right" $ do
          toRight (Pos 'c' 3) `shouldBe` Just (Pos 'd' 3)

        it "toUp returns Nothing at the top edge" $ do
          toUp (Pos 'c' 7) `shouldBe` Nothing

        it "toUp returns the neighbor above" $ do
          toUp (Pos 'c' 6) `shouldBe` Just (Pos 'c' 7)

        it "toDown returns Nothing at the bottom edge" $ do
          toDown (Pos 'c' 0) `shouldBe` Nothing

        it "toDown returns the neighbor below" $ do
          toDown (Pos 'c' 1) `shouldBe` Just (Pos 'c' 0)

      ---------------------------------------------------------------------------
      -- whatIsInPosition
      ---------------------------------------------------------------------------
      describe "whatIsInPosition" $ do
        it "returns the correct cell on the starting board (top row)" $ do
          whatIsInPosition startingBoard (Pos 'a' 7) `shouldBe` Just Queen
          whatIsInPosition startingBoard (Pos 'c' 7) `shouldBe` Just Drone
          whatIsInPosition startingBoard (Pos 'd' 7) `shouldBe` Just Empty

        it "returns the correct cell on the starting board (bottom row)" $ do
          whatIsInPosition startingBoard (Pos 'a' 0) `shouldBe` Just Empty
          whatIsInPosition startingBoard (Pos 'b' 0) `shouldBe` Just Drone
          whatIsInPosition startingBoard (Pos 'd' 0) `shouldBe` Just Queen

        it "returns Nothing for out-of-board positions" $ do
          whatIsInPosition startingBoard (Pos 'e' 0) `shouldBe` Nothing
          whatIsInPosition startingBoard (Pos 'a' 8) `shouldBe` Nothing

      ---------------------------------------------------------------------------
      -- isCellInPosition
      ---------------------------------------------------------------------------
      describe "isCellInPosition" $ do
        it "returns True when the cell matches" $ do
          isCellInPosition Queen startingBoard (Pos 'a' 7) `shouldBe` True
          isCellInPosition Drone startingBoard (Pos 'b' 0) `shouldBe` True

        it "returns False when the cell does not match or out of bounds" $ do
          isCellInPosition Pawn startingBoard (Pos 'a' 7) `shouldBe` False
          isCellInPosition Queen startingBoard (Pos 'e' 0) `shouldBe` False

      ---------------------------------------------------------------------------
      -- isOnCurrentPlayerSide
      ---------------------------------------------------------------------------
      describe "isOnCurrentPlayerSide" $ do
        it "Top side is rows 4..7" $ do
          isOnCurrentPlayerSide Top (Pos 'a' 7) `shouldBe` True
          isOnCurrentPlayerSide Top (Pos 'b' 4) `shouldBe` True
          isOnCurrentPlayerSide Top (Pos 'c' 3) `shouldBe` False
          isOnCurrentPlayerSide Top (Pos 'd' 0) `shouldBe` False

        it "Bottom side is rows 0..3" $ do
          isOnCurrentPlayerSide Bottom (Pos 'a' 0) `shouldBe` True
          isOnCurrentPlayerSide Bottom (Pos 'b' 3) `shouldBe` True
          isOnCurrentPlayerSide Bottom (Pos 'c' 4) `shouldBe` False
          isOnCurrentPlayerSide Bottom (Pos 'd' 7) `shouldBe` False

    ---------------------------------------------------------------------------
    -- More helper-function tests (Logic)
    ---------------------------------------------------------------------------
    describe "More helper functions (Logic)" $ do

      -------------------------------------------------------------------------
      -- crossedCanal
      -------------------------------------------------------------------------
      describe "crossedCanal" $ do
        it "returns False when both positions are on the same side" $ do
          crossedCanal (Pos 'a' 2) (Pos 'd' 3) `shouldBe` False
          crossedCanal (Pos 'a' 6) (Pos 'd' 7) `shouldBe` False

        it "returns True when positions are on different sides (canal crossed)" $ do
          crossedCanal (Pos 'b' 3) (Pos 'b' 4) `shouldBe` True
          crossedCanal (Pos 'c' 4) (Pos 'c' 3) `shouldBe` True

      -------------------------------------------------------------------------
      -- forbiddenTakebackTarget
      -------------------------------------------------------------------------
      describe "forbiddenTakebackTarget" $ do
        it "returns Nothing when there is no last move" $ do
          forbiddenTakebackTarget (Pos 'b' 4) Nothing `shouldBe` Nothing

        it "returns Just start when last move crossed canal and current pos is the last target" $ do
          let lastMove = Just (Move (Pos 'b' 3) (Pos 'b' 4))
          forbiddenTakebackTarget (Pos 'b' 4) lastMove `shouldBe` Just (Pos 'b' 3)

        it "returns Nothing when current pos is not the last move target" $ do
          let lastMove = Just (Move (Pos 'b' 3) (Pos 'b' 4))
          forbiddenTakebackTarget (Pos 'a' 4) lastMove `shouldBe` Nothing

        it "returns Nothing when the last move did not cross the canal" $ do
          let lastMove = Just (Move (Pos 'b' 2) (Pos 'b' 3))
          forbiddenTakebackTarget (Pos 'b' 3) lastMove `shouldBe` Nothing

      -------------------------------------------------------------------------
      -- filterForbiddenTarget
      -------------------------------------------------------------------------
      describe "filterForbiddenTarget" $ do
        it "removes moves that target the forbidden position" $ do
          let moves =
                [ Move (Pos 'a' 0) (Pos 'b' 1)
                , Move (Pos 'a' 0) (Pos 'c' 2)
                , Move (Pos 'a' 0) (Pos 'd' 3)
                ]
          filterForbiddenTarget (Just (Pos 'c' 2)) moves
            `shouldBe`
              [ Move (Pos 'a' 0) (Pos 'b' 1)
              , Move (Pos 'a' 0) (Pos 'd' 3)
              ]

      -------------------------------------------------------------------------
      -- ray
      -------------------------------------------------------------------------
      describe "ray" $ do
        it "generates correct rays in all four orthogonal directions" $ do
          ray (Pos 'b' 3) left  `shouldBe` [Pos 'a' 3]
          ray (Pos 'b' 3) right `shouldBe` [Pos 'c' 3, Pos 'd' 3]
          ray (Pos 'b' 3) up    `shouldBe` [Pos 'b' 4, Pos 'b' 5, Pos 'b' 6, Pos 'b' 7]
          ray (Pos 'b' 3) down  `shouldBe` [Pos 'b' 2, Pos 'b' 1, Pos 'b' 0]

      -------------------------------------------------------------------------
      -- applyRayRules (updated signature + fusion rules)
      -------------------------------------------------------------------------
      describe "applyRayRules" $ do

        it "keeps empty squares until the board edge on an empty board (Drone moving)" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          applyRayRules emptyBoard Bottom Drone (ray (Pos 'b' 3) up)
            `shouldBe`
              [Pos 'b' 4, Pos 'b' 5, Pos 'b' 6, Pos 'b' 7]

        it "stops before own-side piece and does not include it (Drone moving)" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          let b = setCell emptyBoard (Pos 'b' 2) Pawn
          applyRayRules b Bottom Drone (ray (Pos 'b' 3) down)
            `shouldBe` []

        it "includes first opponent-side piece and then stops (Drone moving)" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          let b = setCell emptyBoard (Pos 'b' 4) Pawn
          applyRayRules b Bottom Drone (ray (Pos 'b' 3) up)
            `shouldBe` [Pos 'b' 4]

        it "allows fusion-capture on own side (Pawn onto Drone) ONLY when mover has that figure present" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Bottom side setup:
          -- mover is Pawn (figure=Pawn)
          -- target square has Drone on Bottom side
          -- and Bottom has at least one Pawn somewhere (so presence-check is True)
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 1) Pawn)   -- presence of Pawn on Bottom side
                  (Pos 'b' 2)
                  Drone

          -- ray from b3 down hits b2 first
          applyRayRules b Bottom Pawn (ray (Pos 'b' 3) down)
            `shouldBe` [Pos 'b' 2]

        it "does NOT allow fusion-capture on own side if mover figure is NOT present on that side" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Bottom has NO Pawn anywhere, but there is a Drone at b2.
          -- mover is Pawn (figure=Pawn) => presence-check False => cannot fuse.
          let b = setCell emptyBoard (Pos 'b' 2) Drone

          applyRayRules b Bottom Pawn (ray (Pos 'b' 3) down)
            `shouldBe` []   -- blocked by own-side piece, no fusion allowed

        it "allows fusion-capture on own side (Drone onto Pawn) ONLY when mover has that figure present" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Bottom has a Drone somewhere (presence-check for Drone = True)
          -- target square has Pawn on Bottom side
          let b =
                setCell
                  (setCell emptyBoard (Pos 'a' 0) Drone)  -- presence of Drone on Bottom side
                  (Pos 'b' 2)
                  Pawn

          applyRayRules b Bottom Drone (ray (Pos 'b' 3) down)
            `shouldBe` [Pos 'b' 2]

        it "does NOT allow fusion-capture (Drone onto Pawn) if mover figure is missing on that side" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Bottom has NO Drone anywhere, but there is a Pawn at b2.
          -- mover is Drone (figure=Drone) => presence-check False => cannot fuse.
          let b = setCell emptyBoard (Pos 'b' 2) Pawn

          applyRayRules b Bottom Drone (ray (Pos 'b' 3) down)
            `shouldBe` []   -- blocked by own-side piece, no fusion allowed


      -------------------------------------------------------------------------
      -- isFigurePresentOnPlayerSide (helper used by fusion rules)
      -------------------------------------------------------------------------
      describe "isFigurePresentOnPlayerSide" $ do

        it "returns False on empty board for Pawn/Drone/Queen" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          isFigurePresentOnPlayerSide emptyBoard Top Pawn    `shouldBe` False
          isFigurePresentOnPlayerSide emptyBoard Top Drone   `shouldBe` False
          isFigurePresentOnPlayerSide emptyBoard Top Queen   `shouldBe` False
          isFigurePresentOnPlayerSide emptyBoard Bottom Pawn `shouldBe` False
          isFigurePresentOnPlayerSide emptyBoard Bottom Drone `shouldBe` False
          isFigurePresentOnPlayerSide emptyBoard Bottom Queen `shouldBe` False

        it "returns True only when the figure exists on that player's side" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Put a Pawn on Top side (row 6) and a Drone on Bottom side (row 1)
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 6) Pawn)
                  (Pos 'c' 1)
                  Drone

          isFigurePresentOnPlayerSide b Top Pawn     `shouldBe` True
          isFigurePresentOnPlayerSide b Top Drone    `shouldBe` False
          isFigurePresentOnPlayerSide b Bottom Drone `shouldBe` True
          isFigurePresentOnPlayerSide b Bottom Pawn  `shouldBe` False


      -------------------------------------------------------------------------
      -- targetsToMoves
      -------------------------------------------------------------------------
      describe "targetsToMoves" $ do
        it "converts target positions into moves from the given start position" $ do
          targetsToMoves (Pos 'a' 0) [Pos 'b' 1, Pos 'c' 2]
            `shouldBe`
              [ Move (Pos 'a' 0) (Pos 'b' 1)
              , Move (Pos 'a' 0) (Pos 'c' 2)
              ]

    ---------------------------------------------------------------------------
    -- Move generators (pawn / drone / queen)
    ---------------------------------------------------------------------------
    describe "Move generation (pawn, drone, queen)" $ do

      let emptyBoard = replicate 8 (replicate 4 Empty)

      -------------------------------------------------------------------------
      -- Pawn moves
      -------------------------------------------------------------------------
      describe "pawnMoves" $ do

        it "can move diagonally on an empty board" $ do
          let b = setCell emptyBoard (Pos 'b' 2) Pawn
          pawnMoves b Bottom (Pos 'b' 2) Nothing
            `shouldMatchList`
              [ Move (Pos 'b' 2) (Pos 'a' 3)
              , Move (Pos 'b' 2) (Pos 'c' 3)
              , Move (Pos 'b' 2) (Pos 'a' 1)
              , Move (Pos 'b' 2) (Pos 'c' 1)
              ]

        it "can capture diagonally on the opponent side" $ do
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 3) Pawn)
                  (Pos 'a' 4)
                  Drone
          pawnMoves b Bottom (Pos 'b' 3) Nothing
            `shouldContain`
              [ Move (Pos 'b' 3) (Pos 'a' 4) ]

        it "can land on an own-side piece (fusion square) when rules allow it" $ do
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 3) Pawn)
                  (Pos 'a' 2)
                  Drone
          pawnMoves b Bottom (Pos 'b' 3) Nothing
            `shouldContain`
              [ Move (Pos 'b' 3) (Pos 'a' 2) ]

        it "forbids direct takeback after a canal-crossing move" $ do
          let lastMove = Just (Move (Pos 'a' 3) (Pos 'b' 4))
          let b = setCell emptyBoard (Pos 'b' 4) Pawn

          pawnMoves b Top (Pos 'b' 4) Nothing
            `shouldContain`
              [ Move (Pos 'b' 4) (Pos 'a' 3) ]

          pawnMoves b Top (Pos 'b' 4) lastMove
            `shouldNotContain`
              [ Move (Pos 'b' 4) (Pos 'a' 3) ]

      -------------------------------------------------------------------------
      -- Drone moves
      -------------------------------------------------------------------------
      describe "droneMoves" $ do

        it "can move on an empty board" $ do
          let b = setCell emptyBoard (Pos 'b' 2) Drone
          droneMoves b Bottom (Pos 'b' 2) Nothing
            `shouldContain`
              [ Move (Pos 'b' 2) (Pos 'b' 3) ]

        it "can capture on the opponent side with correct range" $ do
          let b =
                setCell
                  (setCell
                    (setCell emptyBoard (Pos 'b' 2) Drone)
                    (Pos 'b' 4)
                    Pawn)
                  (Pos 'b' 5)
                  Pawn
          droneMoves b Bottom (Pos 'b' 2) Nothing
            `shouldContain`
              [ Move (Pos 'b' 2) (Pos 'b' 4) ]
              
        it "can land on an own-side piece (fusion square) when rules allow it" $ do
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 2) Drone)
                  (Pos 'b' 1)
                  Pawn
          droneMoves b Bottom (Pos 'b' 2) Nothing
            `shouldContain`
              [ Move (Pos 'b' 2) (Pos 'b' 1) ]
        
        
        it "forbids direct takeback after a canal-crossing move" $ do
          let lastMove = Just (Move (Pos 'b' 3) (Pos 'b' 4))
          let b = setCell emptyBoard (Pos 'b' 4) Drone

          droneMoves b Top (Pos 'b' 4) Nothing
            `shouldContain`
              [ Move (Pos 'b' 4) (Pos 'b' 3) ]

          droneMoves b Top (Pos 'b' 4) lastMove
            `shouldNotContain`
              [ Move (Pos 'b' 4) (Pos 'b' 3) ]

      -------------------------------------------------------------------------
      -- Queen moves
      -------------------------------------------------------------------------
      describe "queenMoves" $ do

        it "can move in straight directions on an empty board" $ do
          let b = setCell emptyBoard (Pos 'b' 2) Queen
          let moves = queenMoves b Bottom (Pos 'b' 2) Nothing

          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'b' 3)] -- up
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'b' 1)] -- down
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'a' 2)] -- left
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'd' 2)] -- right

        it "can move diagonally on an empty board" $ do
          let b = setCell emptyBoard (Pos 'b' 2) Queen
          let moves = queenMoves b Bottom (Pos 'b' 2) Nothing

          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'a' 3)] -- up-left
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'c' 3)] -- up-right
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'a' 1)] -- down-left
          moves `shouldContain` [Move (Pos 'b' 2) (Pos 'c' 1)] -- down-right

        it "can capture on the opponent side" $ do
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 2) Queen)
                  (Pos 'b' 4)
                  Pawn
          queenMoves b Bottom (Pos 'b' 2) Nothing
            `shouldContain`
              [ Move (Pos 'b' 2) (Pos 'b' 4) ]

        it "can land on an own-side piece (fusion square) when rules allow it" $ do
          let b =
                setCell
                  (setCell emptyBoard (Pos 'b' 2) Drone)
                  (Pos 'b' 1)
                  Pawn
          droneMoves b Bottom (Pos 'b' 2) Nothing
            `shouldContain`
              [ Move (Pos 'b' 2) (Pos 'b' 1) ]

        it "forbids direct takeback after a canal-crossing move" $ do
          let lastMove = Just (Move (Pos 'b' 3) (Pos 'b' 4))
          let b = setCell emptyBoard (Pos 'b' 4) Queen

          queenMoves b Top (Pos 'b' 4) Nothing
            `shouldContain`
              [ Move (Pos 'b' 4) (Pos 'b' 3) ]

          queenMoves b Top (Pos 'b' 4) lastMove
            `shouldNotContain`
              [ Move (Pos 'b' 4) (Pos 'b' 3) ]

      -------------------------------------------------------------------------
      -- pieceValue
      -------------------------------------------------------------------------
      describe "pieceValue" $ do
        it "returns correct point values for all piece types" $ do
          pieceValue Pawn  `shouldBe` 1
          pieceValue Drone `shouldBe` 2
          pieceValue Queen `shouldBe` 3
          pieceValue Empty `shouldBe` 0

      -------------------------------------------------------------------------
      -- diagonal step helpers (toUpLeft / toUpRight / toDownLeft / toDownRight)
      -------------------------------------------------------------------------
      describe "diagonal step helpers" $ do
        it "returns Just neighbor for diagonal steps inside the board" $ do
          toUpLeft    (Pos 'b' 3) `shouldBe` Just (Pos 'a' 4)
          toUpRight   (Pos 'b' 3) `shouldBe` Just (Pos 'c' 4)
          toDownLeft  (Pos 'b' 3) `shouldBe` Just (Pos 'a' 2)
          toDownRight (Pos 'b' 3) `shouldBe` Just (Pos 'c' 2)

        it "returns Nothing when diagonal step would leave the board" $ do
          toUpLeft    (Pos 'a' 7) `shouldBe` Nothing
          toUpRight   (Pos 'd' 7) `shouldBe` Nothing
          toDownLeft  (Pos 'a' 0) `shouldBe` Nothing
          toDownRight (Pos 'd' 0) `shouldBe` Nothing

      -------------------------------------------------------------------------
      -- isTheFieldEmpty
      -------------------------------------------------------------------------
      describe "isTheFieldEmpty" $ do
        it "returns True when the position contains Empty" $ do
          isTheFieldEmpty startingBoard (Pos 'd' 7) `shouldBe` True

        it "returns False when the position contains a piece" $ do
          isTheFieldEmpty startingBoard (Pos 'a' 7) `shouldBe` False

        it "returns False for out-of-board positions" $ do
          isTheFieldEmpty startingBoard (Pos 'e' 0) `shouldBe` False
          isTheFieldEmpty startingBoard (Pos 'a' 8) `shouldBe` False

      -------------------------------------------------------------------------
      -- setAt (only needed in tests)
      -------------------------------------------------------------------------
      describe "setAt" $ do
        it "returns [] when setting on an empty list" $ do
          setAt 0 'x' [] `shouldBe` ([] :: [Char])

        it "replaces the head when index is 0" $ do
          setAt 0 'x' "abc" `shouldBe` "xbc"

        it "replaces an element when index is > 0" $ do
          setAt 2 'x' "abcd" `shouldBe` "abxd"

        it "does nothing when index is out of range (keeps list unchanged)" $ do
          setAt 10 'x' "abcd" `shouldBe` "abcd"

      -------------------------------------------------------------------------
      -- setCell (only needed in tests)
      -------------------------------------------------------------------------
      describe "setCell" $ do
        it "sets exactly one position and leaves others unchanged" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          let b1 = setCell emptyBoard (Pos 'b' 2) Queen

          whatIsInPosition b1 (Pos 'b' 2) `shouldBe` Just Queen
          whatIsInPosition b1 (Pos 'a' 2) `shouldBe` Just Empty
          whatIsInPosition b1 (Pos 'b' 3) `shouldBe` Just Empty

      -------------------------------------------------------------------------
      -- droneTargets (maxDist branch: max 1 vs >1)
      -------------------------------------------------------------------------
      describe "droneTargets" $ do
        it "uses maxDist > 1 when there are multiple pieces on the ray" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Drone at b2 (Bottom side).
          -- Put two pieces somewhere on the UP ray (b4 and b6).
          -- This makes countPiecesInDirection up == 2, so maxDist == 2.
          let b1 = setCell emptyBoard (Pos 'b' 2) Drone
          let b2 = setCell (setCell b1 (Pos 'b' 4) Pawn) (Pos 'b' 6) Pawn

          droneTargets b2 Bottom (Pos 'b' 2)
            `shouldContain`
              [Pos 'b' 3, Pos 'b' 4]  -- two-step (includes capture square at b4)

      -------------------------------------------------------------------------
      -- makeMove
      -------------------------------------------------------------------------
      describe "makeMove" $ do
        it "moves a piece to an empty target and returns score 0" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)
          let b1 = setCell emptyBoard (Pos 'b' 2) Drone

          let (b2, s) = makeMove b1 (Move (Pos 'b' 2) (Pos 'b' 3))

          s `shouldBe` 0
          whatIsInPosition b2 (Pos 'b' 2) `shouldBe` Just Empty
          whatIsInPosition b2 (Pos 'b' 3) `shouldBe` Just Drone

        it "captures an opponent-side piece, replaces it, and returns the correct score" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Drone captures Queen on the opponent side (row 4+)
          let b1 = setCell (setCell emptyBoard (Pos 'b' 2) Drone) (Pos 'b' 4) Queen
          let (b2, s) = makeMove b1 (Move (Pos 'b' 2) (Pos 'b' 4))

          s `shouldBe` 3
          whatIsInPosition b2 (Pos 'b' 2) `shouldBe` Just Empty
          whatIsInPosition b2 (Pos 'b' 4) `shouldBe` Just Drone

        it "returns score 0 when moving onto an occupied square on the mover's own side" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          -- Start is Bottom side, target is also Bottom side => score must be 0
          let b1 = setCell (setCell emptyBoard (Pos 'b' 2) Drone) (Pos 'b' 3) Queen
          let (b2, s) = makeMove b1 (Move (Pos 'b' 2) (Pos 'b' 3))

          s `shouldBe` 0
          whatIsInPosition b2 (Pos 'b' 2) `shouldBe` Just Empty
          whatIsInPosition b2 (Pos 'b' 3) `shouldBe` Just Drone


        it "does not affect unrelated squares" $ do
          let emptyBoard = replicate 8 (replicate 4 Empty)

          let b1 = setCell (setCell emptyBoard (Pos 'a' 7) Queen) (Pos 'd' 0) Pawn
          let (b2, _) = makeMove b1 (Move (Pos 'a' 7) (Pos 'a' 6))

          whatIsInPosition b2 (Pos 'd' 0) `shouldBe` Just Pawn
    
    ---------------------------------------------------------------------------
    -- playerWon
    ---------------------------------------------------------------------------
    describe "playerWon" $ do

      let emptyBoard :: Board
          emptyBoard = replicate 8 (replicate 4 Empty)

      it "returns Nothing if the next player still has at least one legal move (startingBoard)" $ do
        -- last mover = Top -> next player = Bottom -> Bottom has moves on startingBoard
        playerWon startingBoard Top 0 0 `shouldBe` Nothing

        -- last mover = Bottom -> next player = Top -> Top has moves on startingBoard
        playerWon startingBoard Bottom 0 0 `shouldBe` Nothing

      it "returns Nothing even if scores are uneven, as long as the next player has moves" $ do
        playerWon startingBoard Top 100 0 `shouldBe` Nothing
        playerWon startingBoard Bottom 0 100 `shouldBe` Nothing

      it "returns Just Top if the next player has no moves and Top leads on points" $ do
        -- empty board => next player has no moves
        playerWon emptyBoard Top 5 2 `shouldBe` Just Top
        playerWon emptyBoard Bottom 5 2 `shouldBe` Just Top

      it "returns Just Bottom if the next player has no moves and Bottom leads on points" $ do
        -- empty board => next player has no moves
        playerWon emptyBoard Top 2 5 `shouldBe` Just Bottom
        playerWon emptyBoard Bottom 2 5 `shouldBe` Just Bottom

      it "returns Nothing if the next player has no moves and the score is tied" $ do
        playerWon emptyBoard Top 3 3 `shouldBe` Nothing
        playerWon emptyBoard Bottom 3 3 `shouldBe` Nothing

      it "returns Just Top if Bottom is next and has no pieces (no moves), and Top leads" $ do
        -- Only Top has a Queen -> Bottom has no pieces -> Bottom has no moves
        let b1 :: Board
            b1 = setCell emptyBoard (Pos 'b' 6) Queen

        -- last mover = Top -> next player = Bottom (has no moves)
        playerWon b1 Top 3 0 `shouldBe` Just Top

      it "returns Nothing if Top is next and has a legal move (even if Bottom has no pieces)" $ do
        -- Only Top has a Queen -> Top has moves
        let b1 :: Board
            b1 = setCell emptyBoard (Pos 'b' 6) Queen

        -- last mover = Bottom -> next player = Top (has moves) => game not over
        playerWon b1 Bottom 3 0 `shouldBe` Nothing

