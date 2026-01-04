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

    describe "buildBoard/buildFEN consistency" $ do
      it "buildFEN . buildBoard is identity for a valid FEN" $ do
        let fen = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
        buildFEN (buildBoard fen) `shouldBe` fen

      it "roundtrip works for empty board" $ do
        let fen = "///////"
        buildFEN (buildBoard fen) `shouldBe` fen
