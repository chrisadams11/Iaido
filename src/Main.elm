module Main exposing (..)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Html


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
