module CombatScene.Update.InputLogic exposing (..)

import CombatScene.Model.InputModels exposing (..)


updateInputState : InputFrame -> InputState -> InputState
updateInputState inputFrame inputState =
    { playerInputStates =
        List.map2 updatePlayerInputState inputFrame.playerInputs inputState.playerInputStates
    , inputSources = inputState.inputSources
    , ticks = (inputState.ticks % 100) + 1
    , turnChanged = inputState.ticks >= 100
    }



--Updates a player input state given a new frame of player input data


updatePlayerInputState : PlayerInputFrame -> PlayerInputState -> PlayerInputState
updatePlayerInputState playerInputFrame oldState =
    { playerID = oldState.playerID
    , moveDirection = playerInputFrame.moveKeys
    , attack = playerInputFrame.attackBtn
    }
