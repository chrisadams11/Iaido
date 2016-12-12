module CombatScene.View.View exposing (..)

import CombatScene.Model.ViewModels exposing (..)
import SpriteEngine exposing (..)
import Html exposing (Html)
import List exposing (..)
import Utility exposing (..)


draw : DrawState -> Html msg
draw drawState =
    drawFrame { x = 600, y = 400 } (List.map drawTile drawState.tiles)


drawTile : TileViewModel -> Sprite msg
drawTile tile =
    drawSprite
        "Assets/test_element.png"
        (scaleVector tile.position 60)
        {x=60, y=60}
        20

