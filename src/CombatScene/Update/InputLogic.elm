module CombatScene.Update.InputLogic exposing (..)

import CombatScene.Model.InputModels exposing (..)
import Utility exposing (..)


addPlayerMoveToInputFrame : Vector -> PlayerID -> InputFrame -> InputFrame
addPlayerMoveToInputFrame dir playerID inputFrame =
    case findFirst (\i -> i.playerID == playerID) inputFrame.playerInputs of
        Just playerInputFrame ->
            let
                updatedPlayerInputs =
                    listUpdate
                        (\i -> i.playerID == playerID)
                        inputFrame.playerInputs
                        { playerInputFrame | moveKeys = dir }
            in
                { inputFrame | playerInputs = updatedPlayerInputs }

        Nothing ->
            inputFrame


addPlayerAttackToInputFrame : PlayerID -> InputFrame -> InputFrame
addPlayerAttackToInputFrame playerID inputFrame =
    case findFirst (\i -> i.playerID == playerID) inputFrame.playerInputs of
        Just playerInputFrame ->
            let
                updatedPlayerInputs =
                    listUpdate
                        (\i -> i.playerID == playerID)
                        inputFrame.playerInputs
                        { playerInputFrame | attackBtn = True }
            in
                { inputFrame | playerInputs = updatedPlayerInputs }

        Nothing ->
            inputFrame    


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
