module CombatScene.View.View exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import SpriteEngine exposing (..)
import Html exposing (Html)
import List exposing (..)
import Utility exposing (..)


draw : DrawState -> Html msg
draw drawState =
    drawFrame
        { x = 600, y = 400 }
        (List.append
            (List.map drawTile drawState.tiles)
            (List.map drawPlayer drawState.players)
        )


drawTile : TileViewModel -> Sprite msg
drawTile tile =
    drawSprite
        "Assets/tile.png"
        (scaleVector tile.position 100)
        { x = 100, y = 100 }
        0


drawPlayer : PlayerViewModel -> Sprite msg
drawPlayer player =
    drawSprite
        "Assets/player.png"
        (scaleVector player.position 100)
        { x = 100, y = 100 }
        0
