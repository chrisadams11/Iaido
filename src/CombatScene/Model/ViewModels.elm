module CombatScene.Model.ViewModels exposing (..)

import Utility exposing (..)


type alias DrawState =
    { tiles : List TileViewModel
    , players : List PlayerViewModel
    , timer : Float
    }


type alias TileViewModel =
    { position : Coordinate
    }


type alias PlayerViewModel =
    { position : Coordinate
    }


initialDrawState : DrawState
initialDrawState =
    { tiles = []
    , players = []
    , timer = 0.0
    }
