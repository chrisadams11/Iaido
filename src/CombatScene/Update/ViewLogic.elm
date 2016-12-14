module CombatScene.Update.ViewLogic exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import CombatScene.Model.GameModels exposing (..)
import CombatScene.Model.InputModels exposing (..)


updateDrawState : GameState -> InputState -> DrawState -> DrawState
updateDrawState gameState inputState drawState =
    let
        tiles =
            List.map (\tile -> { position = tile.position }) gameState.board
        players =
            List.map (\player -> { position = player.position }) gameState.players
    in
        { tiles = tiles, players = players }
