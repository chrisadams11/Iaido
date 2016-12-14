module CombatScene.Model.GameModels exposing (..)

import Utility exposing (..)
import List exposing (map)


type alias GameState =
    { board : TileCollection
    , players : List Player
    }



--Definition of the Player actor.


type Status
    = Status_Ready
    | Status_Invulnerability
    | Status_Stun


type alias Player =
    { playerID : PlayerID
    , position : Coordinate
    , iaido : Bool
    , collided : Bool
    , status : List Status
    , hit : Bool
    , momentum :
        { dx : Int
        , dy : Int
        }
    }



--Utility structure that associates a player actor with their corresponding
--player input. Easier than moving them around together in tuples.


type alias Tile =
    { position : Coordinate
    , walls :
        { left : Bool
        , right : Bool
        , top : Bool
        , bottom : Bool
        }
    }


type alias TileCollection =
    List Tile


initialGameState : GameState
initialGameState =
    { board = initialBoard
    , players = initialPlayers
    }


initialBoard : List Tile
initialBoard =
    map
        (\i ->
            { position =
                { x = i % 10, y = floor <| toFloat i / 10 }
            , walls =
                { left = False, right = False, top = False, bottom = False }
            }
        )
        <| List.range 0 99


initialPlayers : List Player
initialPlayers =
    [ { playerID = 1
      , position = { x = 0, y = 0 }
      , iaido = True
      , collided = False
      , status = []
      , hit = False
      , momentum = { dx = 0, dy = 0 }
      }
    , { playerID = 2
      , position = { x = 1, y = 0 }
      , iaido = True
      , collided = False
      , status = []
      , hit = False
      , momentum = { dx = 0, dy = 0 }
      }
    ]
