module Update exposing (Msg(..), countPieces, init,  update, allMoves,
                            winner)

import Model exposing (Agent(..), Model, Board, Move, GameState,
                           Position, Winner(..), initGameState, Piece(..))
import Grid

type alias Player = Piece

type Msg
    = MoveMsg Move -- Player move
    | ChangeAgentMsg Player Agent -- (Change agent of a player)
    | ResetMsg


init : ( Model, Cmd Msg )
init =
    ( { gameState=initGameState
      , potentialMoves=(allMoves initGameState)
      , blackAgent=HumanAgent
      , whiteAgent=AIAgent }
      ,  Cmd.none )

reset : Model -> Model
reset model =
    { model
        | gameState=initGameState
        , potentialMoves=(allMoves initGameState)
    }


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

isHuman : Player -> Model -> Bool
isHuman player model =
    case player of
        WhitePiece -> model.whiteAgent == HumanAgent
        BlackPiece -> model.blackAgent == HumanAgent

{-
Let the board move
-}
move : Move -> GameState -> GameState
move (x, y) state =
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
                [] -> {newState | winner=Just (winner newState)}
                _ -> flippedState
        _ ->
            newState

normalize : (Float, Float) -> (Float, Float)
normalize (blackScore, whiteScore) =
    let sum = blackScore + whiteScore in
    (blackScore / sum, whiteScore / sum)

{-
Evaluates the score of a game state, bigger is better
-}
evaluate : GameState -> Float
evaluate state =
    let (whiteCount, blackCount) = countPieces state.board in
    let (whiteCountWeight, blackCountWeight) =
            (0.01 * (toFloat whiteCount)
            , 0.01 * (toFloat blackCount)) in
    let movesCount = toFloat <| List.length (allMoves state) in
    let (blackScore, whiteScore) =
            case state.winner of
                Just BlackWin -> (1 + blackCountWeight, whiteCountWeight)
                Just WhiteWin -> (blackCountWeight, 1 + whiteCountWeight)
                Just Tie -> (0.5 + blackCountWeight, 0.5 + whiteCountWeight)
                Nothing -> (0.5 + blackCountWeight,
                            0.5 + whiteCountWeight) in
    let (nBlack, nWhite) = normalize (blackScore, whiteScore) in
    case state.currentPlayer of
        WhitePiece -> (nWhite^2 - nBlack^2) + movesCount / 10
        BlackPiece -> (nBlack^2 - nWhite^2) + movesCount / 10

type alias MiniMaxNode = {
        move: Move
       ,state: GameState
       ,score: Float
    }

createNode : GameState -> Move -> MiniMaxNode
createNode oldState m =
    let newState = move m oldState in
    let score = evaluate newState in
    { move=m, state=newState, score=score }

minimax : GameState -> Int -> GameState
minimax state maxDepth =
    let nextNodes =
            List.map (\m -> createNode state m) (allMoves state) in
    let sortedNextNodes =
            List.sortBy .score nextNodes
    in
    case sortedNextNodes of
        [] -> state
        hd :: _ -> hd.state

{-
Let the AI move if the current player is AI, otherwise returns the current model
-}
tryMoveAI : Model -> Model
tryMoveAI model =
    let state = model.gameState in
    if (isHuman model.gameState.currentPlayer model) then
        model
    else
        let newState = minimax state 5 in
        tryMoveAI { model
                      | gameState=newState
                      , potentialMoves=(allMoves newState)}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveMsg (x,y) ->
            let newState = move (x, y) model.gameState in
            let newModel = { model
                               | gameState=newState
                               , potentialMoves=(allMoves newState) } in
            ( tryMoveAI newModel, Cmd.none )
        ChangeAgentMsg player agent ->
            let newModel =
                    case player of
                        BlackPiece -> { model | blackAgent=agent}
                        WhitePiece -> { model | whiteAgent=agent}
            in
                ( tryMoveAI newModel, Cmd.none )
        ResetMsg ->
            (tryMoveAI (reset model), Cmd.none )

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
