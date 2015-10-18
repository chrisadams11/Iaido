module Iaido where

import Set exposing (Set)
import Signal exposing ((<~), (~), Signal)
import Signal
import Time exposing (..)
import Char
import Maybe
import Maybe exposing (withDefault)
import List
import List exposing  ((::))
import Window
import Debug
import Game exposing (..)
import Input exposing (..)
import Graphics exposing (..)

---------------------------------MAIN-------------------------------------
--------------------------------------------------------------------------

type alias WorldState =
  {
    sceneState: SceneState
  }

type SceneState = StartMenuScene (StartMenuSceneState) | BattleScene (BattleSceneState)

type alias StartMenuSceneState = Bool

--The world state consists of the input state,
--the game state,
--and the draw state.
type alias BattleSceneState =
  {
    inputState: InputState,
    gameState: GameState,
    drawState: DrawState
  }

startingWorldState : WorldState
startingWorldState =
  {
    sceneState = startingBattleSceneState
  }

startingBattleSceneState : BattleSceneState
startingBattleSceneState =
  {
    inputState = startingInputState,
    gameState = startingGameState,
    drawState = startingDrawState
  }

startingInputState : InputState
startingInputState =
  {
    playerInputStates = [],
    delta = 0
  }

startingGameState : GameState
startingGameState =
  {
    board = [],
    players = []
  }

startingDrawState : DrawState
startingDrawState = 0

--A stream of world states, formed by folding the updateWorldState function
--over the stream of raw input data starting from the given default state.
worldStream : Signal WorldState
worldStream =
  Signal.foldp updateWorldState inputSignal (startingWorldState)

updateWorldState : Input -> WorldState -> WorldState
updateWorldState input worldState =
  let
    newSceneState = case worldState.sceneState of
      BattleScene battleSceneState       -> updateBattleScene input battleSceneState
      StartMenuScene startMenuSceneState -> False
  in
    {worldState |
      sceneState <- newSceneState}

--Updates the world state with raw input data, moving the world forward one frame.
-- First, the input state is updated with the raw control data
-- Next, the game state is updated with the new input state
-- Finally, the draw state is updated with both the new input state and the new game state
updateBattleScene : Input -> BattleSceneState -> BattleSceneState
updateBattleScene input battleSceneState =
  let
    newInputState = updateInputState input battleSceneState.inputState
    newGameState = updateGameState newInputState battleSceneState.gameState
    newDrawState = updateDrawState newInputState newGameState battleSceneState.drawState
  in
    {battleSceneState |
      inputState <- newInputState,
      gameState <- newGameState,
      drawState <- newDrawState}

--Program main function, maps the render function over the stream of world states
--inside the window
main =
  render <~ Window.dimensions ~ (.sceneState <~ worldStream)
