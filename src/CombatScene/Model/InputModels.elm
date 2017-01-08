module CombatScene.Model.InputModels exposing (..)

import Utility exposing (..)
import Char exposing (KeyCode)


--Stateless game input,
--must be processed into stateful input before it can be used.


type alias InputFrame =
    { playerInputs : List PlayerInputFrame
    }



--Stateless input commands from a single player


type alias PlayerInputFrame =
    { playerID : PlayerID
    , moveKeys : Vector
    , attackBtn : Bool
    }



--Stateful game input


type alias InputState =
    { playerInputStates : List PlayerInputState
    , inputSources : List InputSource
    , ticks : Int
    , turnChanged : Bool
    }



--Stateful input commands from a single player


type alias PlayerInputState =
    { playerID : PlayerID
    , moveDirection : Vector
    , attack : Bool
    }



--A map of data inputs from a keyboard or other input device


type alias InputSource =
    { playerID : PlayerID
    , upKey : KeyCode
    , downKey : KeyCode
    , leftKey : KeyCode
    , rightKey : KeyCode
    , attackKey : KeyCode
    }


initialInputFrame : InputFrame
initialInputFrame =
    { playerInputs =
        [ { playerID = 1
          , moveKeys = zeroVector
          , attackBtn = False
          }
        , { playerID = 2
          , moveKeys = zeroVector
          , attackBtn = False
          }
        ]
    }


--A starting input state, only used for development.
--Later, this needs to be obtained at runtime.


initialInputState : InputState
initialInputState =
    { playerInputStates =
        [ { playerID = 1
          , moveDirection = { x = 0, y = 0 }
          , attack = False
          }
        , { playerID = 2
          , moveDirection = { x = 0, y = 0 }
          , attack = False
          }
        ]
    , inputSources = inputSources
    , ticks = 0
    , turnChanged = False
    }



--A set of input sources, only used for development.
--Later, this needs to be obtained at runtime.


inputSources : List InputSource
inputSources =
    [ { playerID = 1
      , upKey = Char.toCode 'w'
      , downKey = Char.toCode 's'
      , leftKey = Char.toCode 'a'
      , rightKey = Char.toCode 'd'
      , attackKey = Char.toCode 'f'
      }
    , { playerID = 2
      , upKey = Char.toCode 'i'
      , downKey = Char.toCode 'k'
      , leftKey = Char.toCode 'j'
      , rightKey = Char.toCode 'l'
      , attackKey = Char.toCode ';'
      }
    ]
