module View exposing (..)

import TitleScene.TitleScene exposing (..)
import CombatScene.CombatScene exposing (..)
import Model exposing (..)
import Update exposing (..)
import Html exposing (Html, button, div, text, img, canvas, h1, h3)
import Html.App
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model =
    case model of
        Title titleModel ->
            drawTitleScene titleModel |> (Html.App.map (\msg -> TitleMsg msg))

        Combat combatModel ->
            drawCombatScene combatModel |> (Html.App.map (\msg -> CombatMsg msg))


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
