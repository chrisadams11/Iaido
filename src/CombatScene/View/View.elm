module CombatScene.View.View exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html)
import List exposing (..)
import Utility exposing (..)
import SpriteSheet exposing (..)

draw : DrawState -> Html msg
draw drawState =
    collage 600 600
        (List.concat
            [ List.map drawTile drawState.tiles
            , [drawClock drawState.timer]
            , List.map drawPlayer drawState.players
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
        spriteFramePosition = getDrawRectangle player.animationState
    in
        croppedImage 
            (spriteFramePosition.x, spriteFramePosition.y) 
            player.animationState.size.x 
            player.animationState.size.y
            player.animationState.sheet
        |> toForm 
        |> scale (60/50)
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