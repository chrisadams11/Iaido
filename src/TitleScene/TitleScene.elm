module TitleScene.TitleScene exposing (..)

import Html exposing (Html, div)


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
    div [] []
