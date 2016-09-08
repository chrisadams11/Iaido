module View exposing (..)

import Model.ViewModels exposing (..)
import Model exposing (..)
import Update exposing (..)
import Html exposing (Html, button, div, text, img, canvas, h1, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import String


view : Model -> Html Msg
view model =
    case model of
        Title ->
            drawTitle

        Game gameModel ->
            drawGame gameModel.drawState


drawTitle : Html Msg
drawTitle =
    div [] []


divFloatWrapper : Html Msg -> Html Msg
divFloatWrapper elem =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "width", "1600px" )
            , ( "height", "900px" )
            ]
        ]
        [ elem ]


drawGame : DrawState -> Html Msg
drawGame drawState =
    div [] []
