module CombatScene.CombatScene exposing (..)

import CombatScene.Model.InputModels exposing (..)
import CombatScene.Model.GameModels exposing (..)
import CombatScene.Model.ViewModels exposing (..)
import CombatScene.Update.InputLogic exposing (..)
import CombatScene.Update.GameLogic exposing (..)
import CombatScene.Update.ViewLogic exposing (..)
import CombatScene.View.View exposing (..)
import Time exposing (Time)
import Utility exposing (..)
import Html exposing (Html, div)
import Keyboard


type CombatSceneTransition
    = GameOver
    | NoTransition


type CombatSceneMsg
    = TitleBegin
    | Tick Time
    | PlayerMove Vector PlayerID
    | PlayerAttack PlayerID
    | Ignored


type alias CombatModel =
    { inputFrame : InputFrame
    , inputState : InputState
    , gameState : GameState
    , drawState : DrawState
    }


initCombatModel : CombatModel
initCombatModel =
    { inputFrame = newInputFrame
    , inputState = initialInputState
    , gameState = initialGameState
    , drawState = initialDrawState
    }


combatScenesubscriptions : CombatModel -> Sub CombatSceneMsg
combatScenesubscriptions model =
    Sub.batch
        [ Time.every (33 * Time.millisecond) Tick
        , Keyboard.presses
            (\key ->
                let
                    mappedCommand =
                        List.map
                            (\inputSource ->
                                if inputSource.upKey == key then
                                    PlayerMove { x = 0, y = 1 } inputSource.playerID |> Just
                                else if inputSource.downKey == key then
                                    PlayerMove { x = 0, y = -1 } inputSource.playerID |> Just
                                else if inputSource.leftKey == key then
                                    PlayerMove { x = -1, y = 0 } inputSource.playerID |> Just
                                else if inputSource.rightKey == key then
                                    PlayerMove { x = 1, y = 0 } inputSource.playerID |> Just
                                else if inputSource.attackKey == key then
                                    PlayerAttack inputSource.playerID |> Just
                                else
                                    Nothing
                            )
                            model.inputState.inputSources
                            |> filterMaybe
                            |> List.head
                in
                    case mappedCommand of
                        Just msg ->
                            msg

                        Nothing ->
                            Ignored
            )
        ]


updateCombatScene : CombatSceneMsg -> CombatModel -> ( CombatModel, Cmd msg, CombatSceneTransition )
updateCombatScene msg model =
    case msg of
        -- Main game loop
        Tick time ->
            let
                updatedInputState =
                    updateInputState model.inputFrame model.inputState

                updatedGameState =
                    updateGameState updatedInputState model.gameState

                updatedDrawState =
                    updateDrawState model.gameState model.inputState model.drawState
            in
                ( { model
                    | inputFrame = newInputFrame
                    , inputState = updatedInputState
                    , gameState = updatedGameState
                    , drawState = updatedDrawState
                  }
                , Cmd.none
                , NoTransition
                )

        -- Input messages
        PlayerMove dir playerID ->
            ( { model
                | inputFrame = addPlayerMoveToInputFrame dir playerID model.inputFrame
              }
            , Cmd.none
            , NoTransition
            )

        PlayerAttack playerID ->
            ( { model
                | inputFrame = addPlayerAttackToInputFrame playerID model.inputFrame
              }
            , Cmd.none
            , NoTransition
            )

        _ ->
            ( model, Cmd.none, NoTransition )


drawCombatScene : CombatModel -> Html CombatSceneMsg
drawCombatScene model =
    draw model.drawState
