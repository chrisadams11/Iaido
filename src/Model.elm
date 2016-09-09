module Model exposing (..)

import TitleScene.TitleScene exposing (..)
import CombatScene.CombatScene exposing (..)


type Model
    = Title TitleModel
    | Combat CombatModel


initModel : Model
initModel =
    Title initTitleModel
