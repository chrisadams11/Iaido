module Input where

import Set exposing (Set)
import Keyboard exposing (KeyCode)
import Keyboard
import Signal exposing ((<~), (~), Signal)
import Signal
import Time exposing (..)
import Char
import List
import List exposing  ((::))
import Window
import Debug

-------------------------------------------------------------------------------
--------------------------------INPUT------------------------------------------
--The input section encapsulates all functions related to retrieving input    -
--from outside sources, including hardware events, network signals, and time  -
-------------------------------------------------------------------------------

--the time between frames, as close to 30 fps as
--the browser can render
delta =
  inSeconds <~ (fps 30)

type alias InputState =
  {
    playerInputStates: List PlayerInputState,
    delta: Time
  }

type alias PlayerID = Int

type alias PlayerInputState =
  {
    playerID: PlayerID,
    moveDirection: {x:Int, y:Int},
    attack: Bool
  }

updateInputState: Input -> InputState -> InputState
updateInputState input oldState =
  {
    playerInputStates = List.map (uncurry updatePlayerInputState) (List.map2 (,) input.playerInputs oldState.playerInputStates),
    delta = input.delta
  }

updatePlayerInputState: PlayerInput -> PlayerInputState -> PlayerInputState
updatePlayerInputState input oldState =
  {
    playerID = oldState.playerID,
    moveDirection = input.moveDirection,
    attack = input.attack
  }

--The input type wraps up all the external input
--that affects the game state
type alias Input =
  {
    playerInputs: List PlayerInput,
    delta: Time
  }

--A single player's input into the game.
type alias PlayerInput =
  {
    moveDirection: {x:Int, y:Int},
    attack: Bool
  }

playerInput: Bool -> Bool -> Bool -> Bool -> Bool -> PlayerInput
playerInput up down left right attack =
  let
    x = if  | right -> 1
            | left -> -1
            | otherwise -> 0
    y = if  | up -> 1
            | down -> -1
            | otherwise -> 0
    moveDir = {x = x, y = y}
  in
    {moveDirection = moveDir, attack = attack}

type alias InputSource =
  {
    upKey: KeyCode,
    downKey: KeyCode,
    leftKey: KeyCode,
    rightKey: KeyCode,
    attackKey: KeyCode
  }

inputSources: List InputSource
inputSources =
  [
    {
      upKey = 1,
      downKey = 2,
      leftKey = 3,
      rightKey = 4,
      attackKey = 5
    },
    {
      upKey = 6,
      downKey = 7,
      leftKey = 8,
      rightKey = 9,
      attackKey = 10
    }
  ]

processInputs: List InputSource -> Set KeyCode -> List PlayerInput
processInputs inputSources keys  =
  List.map (processInput keys) inputSources

processInput: Set KeyCode -> InputSource -> PlayerInput
processInput keys inputSource =
  playerInput
    (Set.member inputSource.upKey keys)
    (Set.member inputSource.downKey keys)
    (Set.member inputSource.leftKey keys)
    (Set.member inputSource.rightKey keys)
    (Set.member inputSource.attackKey keys)

--Input collects the relevant signals,
--sampling on delta-time. Thus, input
--is retrieved exactly every frame.
inputSignal : Signal Input
inputSignal =
  Signal.sampleOn delta
    <| Input
      <~ ((processInputs inputSources) <~ Keyboard.keysDown)
      ~ delta
