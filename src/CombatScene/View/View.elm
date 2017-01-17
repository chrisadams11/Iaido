module CombatScene.View.View exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import SpriteEngine exposing (..)
import Html exposing (Html)
import List exposing (..)
import Utility exposing (..)


draw : DrawState -> Html msg
draw drawState =
    drawFrame
        {x=600, y=600}
        (List.concat
            [ List.map drawTile drawState.tiles
            , [drawClock drawState.timer]
            , List.map (drawPlayer drawState) drawState.players
            ]
        )


drawTile : TileViewModel -> Html msg
drawTile tile =
    let
        scaledPosition = scaleVector tile.position 60
    in
        drawStaticSprite
            "Assets/Tile.png" 
            {x = scaledPosition.x, y = 600 - scaledPosition.y}
            {x = 60, y = 60}
            0
            []

drawPlayer : DrawState -> PlayerViewModel -> Html msg
drawPlayer drawState player =
    let
        scaledPosition = scaleVector player.position 60
    in
        drawSprite
            player.animationState
            {x = scaledPosition.x, y = 600 - scaledPosition.y}
            0
            (if orientPlayer drawState player 
                then [DrawOption_FlipHorizontal] 
                else [])


orientPlayer : DrawState -> PlayerViewModel -> Bool
orientPlayer drawState player =
    let
        nearestPlayer = if List.length drawState.players < 2 then player else
            List.sortBy 
                (\p -> coordinateDistance p.position player.position) 
                drawState.players
            |> unsafeTail
            |> unsafeHead
    in
        (nearestPlayer.position.x - player.position.x) < 0


drawClock : Float -> Html msg
drawClock timer =
    let 
        timerSprite = if timer < 0.25 then "Assets/TimerStart.png"
                      else if timer < 0.5 then "Assets/TimerQuarter.png"
                      else if timer < 0.75 then "Assets/TimerHalf.png"
                      else "Assets/TimerThreeQuarters.png"
    in
        drawStaticSprite
            timerSprite
            {x=265, y=80}
            {x = 70, y = 70}
            0
            []