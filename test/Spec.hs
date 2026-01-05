-- #############################################################################
-- ###### TESTS                                                       ##########
-- #############################################################################

import Test.Hspec

import Board ( validateFEN, buildBoard, buildFEN
             , Player(Top, Bottom)
             , Cell(Empty, Pawn, Drone, Queen)
             , Pos(Pos)
             )

import Logic (Move(Move), pawnMoves, droneMoves, queenMoves, makeMove, playerWon)

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
