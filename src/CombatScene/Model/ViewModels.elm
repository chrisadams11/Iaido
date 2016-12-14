module CombatScene.Model.ViewModels exposing (..)

import Utility exposing (..)


type alias DrawState =
    { tiles : List TileViewModel
    , players : List PlayerViewModel
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
    }
