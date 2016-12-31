module CombatScene.View.View exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html)
import List exposing (..)
import Utility exposing (..)


draw : DrawState -> Html msg
draw drawState =
    collage 600 600
        (List.append
            (List.map drawTile drawState.tiles)
            (List.map drawPlayer drawState.players)
        )
    |> toHtml


drawTile : TileViewModel -> Form
drawTile tile =
    let
        drawPosition = scaleVector tile.position 60
    in
        image 60 60 "Assets/Tile.png" 
        |> toForm 
        |> move (toFloat drawPosition.x - 270, toFloat  drawPosition.y - 270)


drawPlayer : PlayerViewModel -> Form
drawPlayer player =
    let
        drawPosition = scaleVector player.position 60
    in
        image 60 60 "Assets/Player.png" 
        |> toForm 
        |> move (toFloat drawPosition.x - 270, toFloat drawPosition.y - 270)
