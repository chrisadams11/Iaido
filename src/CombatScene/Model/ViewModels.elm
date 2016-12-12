module CombatScene.Model.ViewModels exposing (..)

import Utility exposing (..)


type alias DrawState =
    { tiles : List TileViewModel
    }


type alias TileViewModel =
    { position : Coordinate
    }


initialDrawState : DrawState
initialDrawState =
    { tiles = []
    }
