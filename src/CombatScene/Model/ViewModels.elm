module CombatScene.Model.ViewModels exposing (..)

import Utility exposing (..)
import Sprite exposing (..)
import Array

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
    , isHit : Bool
    , animationState : Sprite
    }


initialDrawState : DrawState
initialDrawState =
    { tiles = []
    , players = 
        [ { playerID = 1
          , position = {x=0, y=0}
          , isHit = False
          , animationState = playerSprite
          }
        , { playerID = 2
          , position = {x=0, y=0}
          , isHit = False
          , animationState = playerSprite
          }
        ]
    , timer = 0.0
    }


playerSprite : Sprite
playerSprite =
    { sheetFile = "Assets/Player.png"
    , animations =
        [ { frames = -- Idle animation
            [ { drawPos = {x=0,y=0}
              , drawSize = {x = 60, y = 60}
              , duration = 1
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            ] |> Array.fromList
          , isLoop = True
          }
        , { frames = -- Attack animation
            [ { drawPos = {x=0,y=60}
              , drawSize = {x = 60, y = 60}
              , duration = 6
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            , { drawPos = {x=60,y=60}
              , drawSize = {x = 60, y = 60}
              , duration = 8
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            , { drawPos = {x=120,y=60}
              , drawSize = {x = 60, y = 60}
              , duration = 8
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            , { drawPos = {x=180,y=60}
              , drawSize = {x = 60, y = 60}
              , duration = 8
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            ] |> Array.fromList
          , isLoop = False
          }
        , { frames = -- Move animation
            [ { drawPos = {x=0,y=120}
              , drawSize = {x = 60, y = 60}
              , duration = 30
              , disp = Disp_Const {point = {x=0,y=0}}
              }
            ] |> Array.fromList
          , isLoop = False
          }
        ] |> Array.fromList
    , state = 
        { animation = 0
        , frame = 0
        , ticks = 0
        }
    }