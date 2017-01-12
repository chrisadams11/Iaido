module CombatScene.Update.ViewLogic exposing (..)


import Utility exposing (..)
import CombatScene.Model.ViewModels exposing (..)
import CombatScene.Model.GameModels exposing (..)
import CombatScene.Model.InputModels exposing (..)
import SpriteSheet exposing (..)


updateDrawState : GameState -> InputState -> DrawState -> DrawState
updateDrawState gameState inputState drawState =
    let
        tiles =
            List.map (\tile -> { position = tile.position }) gameState.board
        players =
            List.map3 
                (updatePlayerViewModel inputState.turnChanged)
                (List.sortBy (\p -> p.playerID) inputState.playerInputStates)
                (List.sortBy (\p -> p.playerID) gameState.players)
                (List.sortBy (\p -> p.playerID) drawState.players)
    in
        { tiles = tiles
        , players = players
        , timer = toFloat inputState.ticks / toFloat inputState.currentPhase.ticksPerTurn 
        }


updatePlayerViewModel : Bool -> PlayerInputState -> Player -> PlayerViewModel -> PlayerViewModel
updatePlayerViewModel turnChanged inputState gameState oldViewModel =
    let
        attacking = inputState.attack
        moving = inputState.moveDirection /= zeroVector
    in
    { oldViewModel
    | position = gameState.position
    , animationState = 
        if not turnChanged
            then advanceAnimation 1.0 oldViewModel.animationState
        else if attacking
            then enterAnimation 1 oldViewModel.animationState
        else if moving
            then enterAnimation 2 oldViewModel.animationState
        else
            enterAnimation 0 oldViewModel.animationState
    }