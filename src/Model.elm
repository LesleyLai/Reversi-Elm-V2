module Model exposing (Piece(..), Agent(..), Position, Winner(..),
                           Cell, Board, Row, Model, Move, GameState,
                           initGameState)

import Array exposing (Array)
import Grid exposing (Grid)

type Piece = Black | White

type alias Cell = Maybe Piece
type alias Board = Grid Cell
type alias Row = Array Cell

type alias Position = (Int, Int)
type alias Move = Position

type Agent = HumanAgent | AIAgent

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
      , blackAgent: Agent
      , whiteAgent: Agent
    }

initBoard : Board
initBoard =
    Grid.repeat 8 8 Nothing
        |> Grid.set (3, 3) (Just White)
        |> Grid.set (3, 4) (Just Black)
        |> Grid.set (4, 3) (Just Black)
        |> Grid.set (4, 4) (Just White)

initGameState: GameState
initGameState =
      {
          board=initBoard
        , currentPlayer=Black
        , winner=Nothing
      }
