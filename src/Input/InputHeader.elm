module Input where

import Globals exposing (..)
import Set exposing (Set)
import Keyboard
import Keyboard
import Signal exposing (Signal)
import Signal
import Time exposing (..)
import Char exposing (KeyCode)
import Char
import List
import List exposing  ((::))
import Window
import Debug


--Stateful game input
type alias InputState =
  { playerInputStates: List PlayerInputState
  , delta: Time
  }


--Stateful input commands from a single player
type alias PlayerInputState =
  { playerID: PlayerID
  , moveDirection: Vector
  , attack: Bool
  }


--Stateless game input,
--must be processed into stateful input before it can be used.
type alias InputFrame =
  { playerInputs: List PlayerInputFrame
  , delta: Time
  }


--Stateless input commands from a single player
type alias PlayerInputFrame =
  { playerID: PlayerID
  , moveKeys: Vector
  , attackBtn: Bool
  }


--A map of data inputs from a keyboard or other input device
type alias InputSource =
  { playerID: PlayerID
  , upKey: KeyCode
  , downKey: KeyCode
  , leftKey: KeyCode
  , rightKey: KeyCode
  , attackKey: KeyCode
  }


--A starting input state, only used for development.
--Later, this needs to be obtained at runtime.
startingInputState : InputState
startingInputState =
  { playerInputStates = []
  , delta = 0
  }


--A set of input sources, only used for development.
--Later, this needs to be obtained at runtime.
inputSources: List InputSource
inputSources =
  [
    { playerID = 1
    , upKey = 1
    , downKey = 2
    , leftKey = 3
    , rightKey = 4
    , attackKey = 5
    },
    { playerID = 2
    , upKey = 6
    , downKey = 7
    , leftKey = 8
    , rightKey = 9
    , attackKey = 10
    }
  ]
