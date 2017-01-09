module CombatScene.Update.ViewLogic exposing (..)


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
            List.map2 
                updatePlayerViewModel 
                (List.sortBy (\p -> p.playerID) gameState.players)
                (List.sortBy (\p -> p.playerID) drawState.players)
    in
        { tiles = tiles
        , players = players
        , timer = toFloat inputState.ticks / toFloat inputState.currentPhase.ticksPerTurn 
        }


updatePlayerViewModel : Player -> PlayerViewModel -> PlayerViewModel
updatePlayerViewModel playerState oldViewModel =
    { oldViewModel
    | position = playerState.position
    , animationState = advanceAnimation 1.0 oldViewModel.animationState
    }