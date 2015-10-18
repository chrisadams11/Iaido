module Graphics where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Set exposing (Set)
import Signal exposing ((<~), (~), Signal)
import Signal
import Char
import List
import List exposing  ((::))
import Window
import Debug
import Game exposing (..)
import Input exposing (..)


-------------------------------------------------------------------------------
--------------------------------VIEW-------------------------------------------
--The view encapsulates all rendering logic that shows our game state to the  -
--player. This section is entirely self-contained, and is only referenced once-
--outside of itself; in the main function. Because of this constraint, our    -
--game state becomes completely independent of how it is rendered, and the    -
--engine that renders it is entirely hot-swappable.                           -
-------------------------------------------------------------------------------

type alias DrawState = Int

updateDrawState : InputState -> GameState -> DrawState -> DrawState
updateDrawState inputState gameState oldDrawState =
  oldDrawState

--The view for Iaido is stateful, so we have the Animation State for the last
--frame generate the next frame. UpdateAnimation will transition the animation state
--from one frame to the next, then RenderGame will draw that animation state to the screen.

--The view function renders the given game state within the given window.
render : (Int,Int) -> SceneState -> Element
render (w,h) scene =
  case scene of
    BattleScene battleSceneState -> renderBattleScene (w,h) battleSceneState.drawState
    _                       -> rect 100 100

renderBattleScene : (Int, Int) -> DrawState -> Element
renderBattleScene (w,h) drawState =
  rect 200 200
