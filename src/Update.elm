module Update exposing (..)

import Model exposing (..)
import CombatScene.CombatScene exposing (..)
import TitleScene.TitleScene exposing (..)
import Platform.Sub


type Msg
    = CombatMsg CombatSceneMsg
    | TitleMsg TitleSceneMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Combat combatModel ->
            (combatScenesubscriptions combatModel) |> (Sub.map (\sub -> CombatMsg sub))

        Title titleModel ->
            (titleScenesubscriptions titleModel) |> (Sub.map (\sub -> TitleMsg sub))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Title titleModel ->
            case msg of
                TitleMsg titleMsg ->
                    let
                        ( updatedTitleModel, cmd, transition ) =
                            updateTitle titleMsg
                    in
                        case transition of
                            BeginPlay ->
                                ( Combat initCombatModel, Cmd.none )

                            _ ->
                                ( Title updatedTitleModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Combat combatModel ->
            case msg of
                CombatMsg combatMsg ->
                    let
                        ( updatedCombatModel, cmd, transition ) =
                            updateCombatScene combatMsg combatModel
                    in
                        case transition of
                            GameOver ->
                                ( Title initTitleModel, cmd )

                            _ ->
                                ( Combat updatedCombatModel, cmd )

                _ ->
                    ( model, Cmd.none )
