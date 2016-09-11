module TitleScene.TitleScene exposing (..)

import Html exposing (Html, div, img, h1, h3, text)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
import Utility exposing (..)


type TitleSceneTransition
    = BeginPlay
    | Settings
    | NoTransition


type TitleSceneMsg
    = TitleBegin
    | SomethingElse


type alias TitleModel =
    {}


initTitleModel : TitleModel
initTitleModel =
    {}


titleScenesubscriptions : TitleModel -> Sub TitleSceneMsg
titleScenesubscriptions model =
    Sub.none


updateTitle : TitleSceneMsg -> ( TitleModel, Cmd msg, TitleSceneTransition )
updateTitle msg =
    case msg of
        TitleBegin ->
            ( {}, Cmd.none, BeginPlay )

        _ ->
            ( {}, Cmd.none, NoTransition )


drawTitleScene : TitleModel -> Html TitleSceneMsg
drawTitleScene model =
    div []
        [ titleBackground
        , titleBanner
        , titleBegin
        ]


titleBackground : Html TitleSceneMsg
titleBackground =
    img
        [ src "Assets/title_background.png"
        , style []
        ]
        []


titleBanner : Html TitleSceneMsg
titleBanner =
    divFloatWrapper
        (h1
            [ style
                [ ( "margin-left", "auto" )
                , ( "margin-right", "auto" )
                , ( "margin-top", "300px" )
                , ( "text-align", "center" )
                ]
            ]
            [ text "Iaido" ]
        )


titleBegin : Html TitleSceneMsg
titleBegin =
    divFloatWrapper
        (h3
            [ onClick TitleBegin
            , style
                [ ( "margin-left", "auto" )
                , ( "margin-right", "auto" )
                , ( "margin-top", "500px" )
                , ( "width", "50px" )
                , ( "cursor", "pointer" )
                ]
            ]
            [ text "Begin" ]
        )
