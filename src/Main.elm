module Main exposing (..)

import Browser
import Html exposing (Html)

import Model exposing (Model)
import Update exposing (Msg, init, update)
import View exposing (view)

---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
