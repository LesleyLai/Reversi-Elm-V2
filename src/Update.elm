module Update exposing (Msg(..), countPieces, init,  update, allMoves,
                            winner)

import Model exposing (Agent(..), Model, Board, Move, GameState,
                           Position, Winner(..), initGameState, Piece(..))
import Grid

type alias Player = Piece

type Msg
    = MoveMsg Move -- Player move
    | ChangeAgent Player Agent -- (Change agent of a player)


init : ( Model, Cmd Msg )
init =
    ( { gameState=initGameState
      , potentialMoves=(allMoves initGameState)
      , blackAgent=HumanAgent
      , whiteAgent=AIAgent }
      ,  Cmd.none )

nextPlayer : Player -> Player
nextPlayer player =
    case player of
        WhitePiece -> BlackPiece
        BlackPiece -> WhitePiece

{-
Flips the player in a game state
-}
flipPlayer : GameState -> GameState
flipPlayer state =
    {state | currentPlayer=(nextPlayer state.currentPlayer)}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg (x,y) ->
            let state = model.gameState in
            let board = state.board
                player = state.currentPlayer in
            let pieceToFlip = getSandwiches (x,y) state
                            |> List.concat in
            let newBoard = List.foldl
                           (\(xx, yy) b
                                -> Grid.set (xx, yy) (Just player) b)
                            board pieceToFlip
                            |> Grid.set (x, y) (Just player) in
            let newState = {board=newBoard
                          , currentPlayer=(nextPlayer player)
                          , winner=Nothing} in
            let newMoves=(allMoves newState) in
            case newMoves of
               [] ->
                   let flippedState = flipPlayer newState in
                   let flippedMoves = allMoves flippedState in
                   case flippedMoves of
                       [] -> ({model |
                              gameState={newState | winner=Just (winner newState)}
                            , potentialMoves=[]}, Cmd.none)
                       _ -> ({model | gameState=flippedState,
                              potentialMoves=flippedMoves} , Cmd.none )
               _ ->
                    let nextModel = {model | gameState=newState
                           , potentialMoves=newMoves} in
                    ( nextModel, Cmd.none )
        ChangeAgent player agent ->
            case player of
                BlackPiece -> ( { model | blackAgent=agent}, Cmd.none )
                WhitePiece -> ( { model | whiteAgent=agent}, Cmd.none )

{-
Given a board, returns (whiteCount, blackCount)
-}
countPieces : Board -> (Int, Int)
countPieces board =
    Grid.foldl (\piece (w,b) ->
                              case piece of
                                  Nothing -> (w, b)
                                  Just BlackPiece -> (w, b+1)
                                  Just WhitePiece -> (w+1, b)
                        ) (0,0) board

{-
Gets the winner of the current board configuration.
Precondition: the game should end already
-}
winner : GameState -> Winner
winner state =
    let board = state.board in
    let (whiteCount, blackCount) = countPieces board in
    if (whiteCount > blackCount) then
        WhiteWin
    else if (whiteCount < blackCount) then
        BlackWin
    else
        Tie

-- Game logics
type Direction = Direction (Int, Int)

-- Sandwiches are positions of the need to flip
-- An empty list means no sandwich
type alias Sandwich = List Position

allMoves : GameState -> List Move
allMoves state =
    let board = state.board
        player = state.currentPlayer
    in
    List.range 0 (Grid.height board - 1)
        |> List.concatMap
           (\y -> (List.range 0 (Grid.width board - 1))
           |> List.map (\x -> (x, y)))
           |> List.filter (\pos ->
                               (Grid.get pos board) == Just Nothing)
           |> List.filterMap
              (\pos ->
                   case (getSandwiches pos state) of
                       [] -> Nothing
                       _ -> Just pos
              )

getSandwich : GameState -> Position -> Direction -> Sandwich -> Sandwich
getSandwich state pos dir acc =
    let board = state.board
        player = state.currentPlayer
        nextPos = addDirection pos dir
    in
        case (Grid.get nextPos board) of
            Nothing -> [] -- Out of index
            Just Nothing -> [] -- Empty
            Just (Just piece) ->
                if (piece == player) then
                    acc
                else
                    getSandwich state nextPos dir (nextPos :: acc)

-- Returns the position of sandwiches
getSandwiches : Position -> GameState -> List (Sandwich)
getSandwiches pos state =
    let board = state.board in
    neighbors pos board
    |> List.filterMap
       (\neighborPos ->
            let dir = getDirection pos neighborPos in
            let sandwich = getSandwich state pos dir [] in
            case sandwich of
                [] -> Nothing
                _ -> Just sandwich
       )


addDirection : Position -> Direction -> Position
addDirection (x, y) (Direction(dx, dy)) =
    (x + dx, y + dy)

-- Get the direction from the first point to the second point
getDirection : Position -> Position -> Direction
getDirection (x1, y1) (x2, y2) =
    Direction(x2 - x1, y2 - y1)

directions : List Direction
directions =
    [(-1, -1), (0, -1), (1, -1), (-1, 0),
         (1, 0), (-1, 1), (0, 1), (1, 1)]
        |> List.map Direction

neighbors : Position -> Board -> List Position
neighbors pos board =
    List.map (\dir -> addDirection pos dir) directions
        |> List.filter (\(newX, newY) ->
                            newX >= 0 && newY >= 0
                            && newX <= (Grid.width board)
                            && newY <= (Grid.height board))
