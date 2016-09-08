module Update.GameLogic exposing (..)

import Model.InputModels exposing (..)
import Model.GameModels exposing (..)
import Update.ActionSystem exposing (..)
import Update.GameUtilities exposing (..)
import Utility exposing (..)

updateGameState : InputState -> GameState -> GameState
updateGameState inputState gameState =
    if inputState.turnChanged then
        playTurn inputState gameState
    else
        gameState


playTurn : InputState -> GameState -> GameState
playTurn input oldGame =
    let
        actions =
            decideActions input oldGame

        effectQueue =
            doActions actions oldGame

        newGame =
            resolveEffectQueue effectQueue oldGame
    in
        newGame


decideActions : InputState -> GameState -> List Action
decideActions inputState gameState =
    let
        playersWithInputs =
            (associatePlayersWithInputs inputState.playerInputStates gameState.players)
    in
        List.map decideAction playersWithInputs


decideAction : PlayerWithInput -> Action
decideAction playerWithInput =
    let
        player =
            playerWithInput.player

        input =
            playerWithInput.inputState
    in
        if input.attack then
            if listContains player.status Status_Ready then
                Action_Iaido (IaidoAction player input.moveDirection)
            else
                Action_Attack (AttackAction player input.moveDirection)
        else if input.moveDirection /= zeroVector then
            Action_Move (MoveAction player input.moveDirection)
        else
            Action_Ready (ReadyAction player)
