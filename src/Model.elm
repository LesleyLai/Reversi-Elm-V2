module Model exposing (Piece(..), Position, Winner(..),
                           Cell, Board, Row, Model, Move, GameState,
                           initGameState)

import Array exposing (Array)
import Grid exposing (Grid)

type Piece = BlackPiece | WhitePiece

type alias Cell = Maybe Piece
type alias Board = Grid Cell
type alias Row = Array Cell

type alias Position = (Int, Int)
type alias Move = Position

type Winner = BlackWin | WhiteWin | Tie

type alias GameState
    = {
          board: Board
        , currentPlayer: Piece
        , winner: Maybe Winner
      }

type alias Model =
    {
        gameState: GameState
      , potentialMoves: List Move
    }

initBoard : Board
initBoard =
    Grid.repeat 8 8 Nothing
        |> Grid.set (3, 3) (Just WhitePiece)
        |> Grid.set (3, 4) (Just BlackPiece)
        |> Grid.set (4, 3) (Just BlackPiece)
        |> Grid.set (4, 4) (Just WhitePiece)

initGameState: GameState
initGameState =
      {
          board=initBoard
        , currentPlayer=BlackPiece
        , winner=Nothing
      }
