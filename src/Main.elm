module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Html.App as App

        
main : Program Never
main =
    App.program
        { init = ( initModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }