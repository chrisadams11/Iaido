module Model.InputModels exposing (..)

import Utility exposing (..)
import Char exposing (KeyCode)
import Char


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

newInputFrame : InputFrame
newInputFrame =
    { playerInputs = []
    }

    --A starting input state, only used for development.
--Later, this needs to be obtained at runtime.


initialInputState : InputState
initialInputState =
    { playerInputStates = []
    , ticks = 0
    , turnChanged = False
    }



--A set of input sources, only used for development.
--Later, this needs to be obtained at runtime.


inputSources : List InputSource
inputSources =
    [ { playerID = 1
      , upKey = 1
      , downKey = 2
      , leftKey = 3
      , rightKey = 4
      , attackKey = 5
      }
    , { playerID = 2
      , upKey = 6
      , downKey = 7
      , leftKey = 8
      , rightKey = 9
      , attackKey = 10
      }
    ]
