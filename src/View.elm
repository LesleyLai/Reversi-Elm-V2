module View exposing (view)

import Model exposing (Agent(..), GameState, Piece(..), Position, Winner(..),
                           Cell, Board, Row, Model, Move)

import FontAwesome.Icon
import FontAwesome.Solid

import Array exposing (Array)
import Grid
import Html exposing (Html, article, span, section, text, div, h1, img, p)
import Html.Attributes exposing (class, classList, style, src)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Svg.Attributes
import Update exposing (Msg(..), countPieces, init, update)

type alias MovesSet = Set Position

cellView : Position -> MovesSet -> (Maybe Piece) -> Html Msg
cellView pos movesMap cell =
    let pieceView piece =
            div [classList [
                      ("piece", True)
                     ,("black", piece == BlackPiece)
                     ,("white", piece == WhitePiece)
                     ]] [] in
    let cellAttributes =
            if Set.member pos movesMap then
                [ classList [("cell", True)
                            ,("movable-cell", True)]
                , onClick (MoveMsg pos)]
            else
                [classList [("cell", True)]]
    in
    div cellAttributes [
         (case cell of
              Nothing -> div [] []
              Just piece -> pieceView piece
         )
        ]

rowView : Int -> MovesSet -> Array (Maybe Piece) -> Html Msg
rowView y movesMap row =
    div [ style "display" "flex" ]
        ((Array.indexedMap (\x cell -> cellView (x,y) movesMap cell) row)
        |> Array.toList)

boardView : Board -> MovesSet -> Html Msg
boardView board movesMap =
    let rows = Grid.rows board in
    div [ style "margin" "auto", style "width" "400px" ]
        ((Array.indexedMap (\y row -> rowView y movesMap row) rows)
            |> Array.toList)

currentPlayerView: GameState -> Html Msg
currentPlayerView state =
    case state.winner of
        Nothing ->
            div [] [ text <| "Current player: " ++
                         (if (state.currentPlayer == WhitePiece)
                          then "White"
                          else "Black")]
        Just BlackWin -> div [] [ text <| "Black Win"]
        Just WhiteWin -> div [] [ text <| "White Win"]
        Just Tie -> div [] [ text <| "Tie"]


agentStatusView: Model -> Piece -> Int -> Html Msg
agentStatusView model player pieceCount =
    let (isHuman, name) =
            case player of
                BlackPiece -> (model.blackAgent == HumanAgent, "Black Player")
                WhitePiece -> (model.whiteAgent == HumanAgent, "White Player")
    in
    let role =
            if isHuman then
                "(Human)"
            else
                "(AI)"
    in
    let (humanClass, aiClass) = if isHuman then
                         ("agent-icon-active", "agent-icon-inactive")
                     else
                         ("agent-icon-inactive", "agent-icon-active")
    in
        p [ classList [("agent", True)] ]
            [ div [ classList [("agent-icons", True)] {-onClick Decrement-} ]
                  [ FontAwesome.Icon.viewStyled
                        [Svg.Attributes.class "agent-icon"
                        , Svg.Attributes.class aiClass]
                        FontAwesome.Solid.robot
                  , FontAwesome.Icon.viewStyled
                      [Svg.Attributes.class "agent-icon"
                      , Svg.Attributes.class humanClass]
                      FontAwesome.Solid.user ],
                  text <| name ++ role
                  ++ " Score: " ++ (String.fromInt pieceCount) ]
    

scoresView: Model -> Html Msg
scoresView model =
    let (whiteCount, blackCount) = countPieces model.gameState.board in
    article []
        [ 
          agentStatusView model BlackPiece blackCount,
          agentStatusView model WhitePiece whiteCount
        ]


view : Model -> Html Msg
view model =
    let movesSet = List.foldl
                   (\move set ->
                        Set.insert move set
                   ) (Set.empty) model.potentialMoves
    in
    div []
        [ h1 [] [ text "Reversi-Elm-V2" ]
        , boardView model.gameState.board movesSet
        , currentPlayerView model.gameState
        , scoresView model
        ]
