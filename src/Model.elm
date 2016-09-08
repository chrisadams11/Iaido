module Model exposing (..)

import Utility exposing (..)
import Model.InputModels exposing (..)
import Model.GameModels exposing (..)
import Model.ViewModels exposing (..)
import Time exposing (..)

type Model
    = Title
    | Game GameModel


type alias GameModel =
    { inputFrame : InputFrame
    , inputState : InputState
    , gameState : GameState
    , drawState : DrawState
    }


initModel : Model
initModel =
    Title


initGameModel : Model
initGameModel =
    Game
        { inputFrame = newInputFrame
        , inputState = initialInputState
        , gameState = initialGameState
        , drawState = {}
        }