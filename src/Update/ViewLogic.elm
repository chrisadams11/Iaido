module Update.ViewLogic exposing (..)

import Model.ViewModels exposing (..)
import Model.GameModels exposing (..)
import Model.InputModels exposing (..)

updateDrawState : GameState -> InputState -> DrawState -> DrawState
updateDrawState gameState inputState drawState =
    drawState