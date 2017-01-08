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
        (List.concat
            [ List.map drawTile drawState.tiles
            , List.map drawPlayer drawState.players
            , [drawClock drawState.timer]
            ]
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


drawClock : Float -> Form
drawClock timer =
    let 
        timerSprite = if timer < 0.25 then "Assets/TimerStart.png"
                      else if timer < 0.5 then "Assets/TimerQuarter.png"
                      else if timer < 0.75 then "Assets/TimerHalf.png"
                      else "Assets/TimerThreeQuarters.png"
    in
        image 70 70 timerSprite
        |> toForm
        |> move (0, toFloat 200)