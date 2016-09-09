module CombatScene.CombatScene exposing (..)

import CombatScene.Model.InputModels exposing (..)
import CombatScene.Model.GameModels exposing (..)
import CombatScene.Model.ViewModels exposing (..)
import CombatScene.Update.InputLogic exposing (..)
import CombatScene.Update.GameLogic exposing (..)
import CombatScene.Update.ViewLogic exposing (..)
import Time exposing (..)
import Html exposing (Html, div)


type CombatSceneTransition
    = GameOver
    | NoTransition


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
    , drawState = {}
    }


type CombatSceneMsg
    = TitleBegin
    | Tick Time


combatScenesubscriptions : CombatModel -> Sub CombatSceneMsg
combatScenesubscriptions model =
    Sub.none


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
        _ ->
            ( model, Cmd.none, NoTransition )


drawCombatScene : CombatModel -> Html CombatSceneMsg
drawCombatScene model =
    div [] []
