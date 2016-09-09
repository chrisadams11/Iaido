module CombatScene.Model.GameModels exposing (..)

import Utility exposing (..)


type alias GameState =
    { board : List Tile
    , players : List Player
    }



--Definition of the Player actor.


type Status
    = Status_Ready
    | Status_Invulnerability
    | Status_Stun


type alias Player =
    { playerID : PlayerID
    , tile : Tile
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
    { position : { row : Int, col : Int }
    , walls :
        { left : Bool
        , right : Bool
        , top : Bool
        , bottom : Bool
        }
    }


initialGameState : GameState
initialGameState =
    { board = []
    , players = []
    }
