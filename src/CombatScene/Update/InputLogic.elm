module CombatScene.Update.InputLogic exposing (..)

import CombatScene.Model.InputModels exposing (..)
import Utility exposing (..)


addPlayerMoveToInputFrame : Vector -> PlayerID -> InputFrame -> InputFrame
addPlayerMoveToInputFrame dir playerID inputFrame =
    case findFirst (\i -> i.playerID == playerID) inputFrame.playerInputs of
        Just playerInputFrame ->
            let
                updatedPlayerDirX =
                    if dir.x == 0 then
                        playerInputFrame.moveKeys.x
                    else
                        dir.x

                updatedPlayerDirY =
                    if dir.y == 0 then
                        playerInputFrame.moveKeys.y
                    else
                        dir.y
            in
                { inputFrame
                    | playerInputs =
                        listUpdate
                            (\i -> i.playerID == playerID)
                            inputFrame.playerInputs
                            { playerInputFrame | moveKeys = { x = updatedPlayerDirX, y = updatedPlayerDirY } }
                }

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
    if inputState.turnChanged then
        if inputState.currentPhase.turnsInPhase - inputState.turnCount <= 1 
            && List.length inputState.remainingPhases > 0 then
            { inputState
                | playerInputStates =
                    List.map clearPlayerInputState inputState.playerInputStates
                , turnCount = 0
                , currentPhase = unsafeHead inputState.remainingPhases
                , remainingPhases = unsafeTail inputState.remainingPhases
                , ticks = 0
                , turnChanged = False
            }
        else
            { inputState
                | playerInputStates =
                    List.map clearPlayerInputState inputState.playerInputStates
                , turnCount = inputState.turnCount + 1
                , ticks = 0
                , turnChanged = False
            }
    else
        { inputState
            | playerInputStates =
                List.map2 updatePlayerInputState 
                    (List.sortBy (\i -> i.playerID) inputFrame.playerInputs) 
                    (List.sortBy (\i -> i.playerID) inputState.playerInputStates)
            , ticks = inputState.ticks + 1
            , turnChanged = inputState.ticks >= inputState.currentPhase.ticksPerTurn
        }


--Updates a player input state given a new frame of player input data


updatePlayerInputState : PlayerInputFrame -> PlayerInputState -> PlayerInputState
updatePlayerInputState playerInputFrame oldState =
    { oldState
        | moveDirection =
            { x =
                if playerInputFrame.moveKeys.x == 0 then
                    oldState.moveDirection.x
                else
                    playerInputFrame.moveKeys.x
            , y =
                if playerInputFrame.moveKeys.y == 0 then
                    oldState.moveDirection.y
                else
                    playerInputFrame.moveKeys.y
            }
        , attack = oldState.attack || playerInputFrame.attackBtn
    }


clearPlayerInputState : PlayerInputState -> PlayerInputState
clearPlayerInputState inputState =
    { inputState
        | moveDirection = zeroVector
        , attack = False
    }


clearInputFrame : InputFrame -> InputFrame
clearInputFrame inputFrame =
    { inputFrame | playerInputs = List.map (\playerInputFrame -> { playerInputFrame | moveKeys = zeroVector, attackBtn = False }) inputFrame.playerInputs }
