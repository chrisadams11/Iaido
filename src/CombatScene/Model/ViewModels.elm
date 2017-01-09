module CombatScene.Model.ViewModels exposing (..)

import Utility exposing (..)
import SpriteSheet exposing (..)


type alias DrawState =
    { tiles : List TileViewModel
    , players : List PlayerViewModel
    , timer : Float
    }


type alias TileViewModel =
    { position : Coordinate
    }


type alias PlayerViewModel =
    { playerID : PlayerID
    , position : Coordinate
    , animationState : Sprite
    }


initialDrawState : DrawState
initialDrawState =
    { tiles = []
    , players = 
        [ { playerID = 1
          , position = {x=0, y=0}
          , animationState = initialPlayerAnimation
          }
        , { playerID = 2
          , position = {x=0, y=0}
          , animationState = initialPlayerAnimation
          }
        ]
    , timer = 0.0
    }


initialPlayerAnimation : Sprite
initialPlayerAnimation = 
    { sheet = "Assets/Player.png"
    , animations =
      [ playerIdleAnimation ]
    , currentAnimation = playerIdleAnimation
    , size = {x=50,y=50}
    , currentFrame = {x=15, y=0}
    }


playerIdleAnimation : SpriteAnimation
playerIdleAnimation = 
    { frames = 30
    , isLoop = True
    }