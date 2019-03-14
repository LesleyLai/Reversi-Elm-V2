module LogicTest exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Expect

import Set

import Grid exposing (Grid)
import Model exposing (Piece(..), Board, GameState, initGameState)
import Update exposing (Winner(..), allMoves, winner)

-- Test the game logic

buildState : Piece -> Board -> GameState
buildState currentPlayer board  =
    {board=board, currentPlayer=currentPlayer}

intsToBoard : Grid Int -> Grid (Maybe Piece)
intsToBoard ig =
    Grid.map (\n -> if (n == 1) then
                        Just BlackPiece
                    else if (n == 2) then
                        Just WhitePiece
                    else Nothing) ig

getMovesTest : Test
getMovesTest =
    describe "getValidMoves test"
        [ test "Valid moves for the initial board" <|
            \_ ->
              let expectMoves = [(2, 3), (3, 2), (4, 5), (5, 4)] in
                Expect.equal (Set.fromList expectMoves)
                    <| Set.fromList (allMoves initGameState)
        ]

endGameTest =
    describe "End Game test"
        [ test "End the game with the full board" <|
            \_ ->
              let endBoard = Grid.fromList ([
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1]
                              ]) |> Maybe.map intsToBoard in
              let state = Maybe.map (buildState BlackPiece) endBoard in
                Expect.equal (Just BlackWin)
                    <| (Maybe.map winner state)
        , test "End the game when no move is possible" <|
            \_ ->
              let endBoard = Grid.fromList ([
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [2, 1, 1, 1, 1, 1, 1, 1],
                                   [2, 2, 1, 1, 1, 1, 1, 1],
                                   [2, 2, 2, 2, 2, 2, 2, 0]
                              ]) |> Maybe.map intsToBoard in
              let state = Maybe.map (buildState BlackPiece) endBoard in
                Expect.equal (Just BlackWin)
                    <| (Maybe.map winner state)
        , test "Ties" <|
            \_ ->
              let endBoard = Grid.fromList ([
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [1, 1, 1, 1, 1, 1, 1, 1],
                                   [2, 2, 2, 2, 2, 2, 2, 2],
                                   [2, 2, 2, 2, 2, 2, 2, 2],
                                   [2, 2, 2, 2, 2, 2, 2, 2],
                                   [2, 2, 2, 2, 2, 2, 2, 2]
                              ]) |> Maybe.map intsToBoard in
              let state = Maybe.map (buildState BlackPiece) endBoard in
                Expect.equal (Just Tie)
                    <| (Maybe.map winner state)
        ]
