module Game where

import Set exposing (Set)
import Signal exposing ((<~), (~), Signal)
import Signal
import Time exposing (..)
import Char
import Maybe
import Maybe exposing (withDefault)
import List
import List exposing  ((::))
import Debug
import Input exposing (..)

---------------------------------GAMESTATE-------------------------------------
-------------------------------------------------------------------------------

--The game is just the collection of all actors, with a state.
type alias GameState =
  {
    board:List Tile,
    players:List Player
  }

type alias Player =
  {
    playerID: PlayerID,
    tile: Tile,
    iaido: Bool,
    collided: Bool,
    momentum:
      {
        dx: Int,
        dy: Int
      }
  }

type alias PlayerID = Int

--Updates the game state given the input.
--Each frame consists of an indeterminate number of "phases."
--Each phase is the mapping of a particular function with type TurnPhase
--over the collection of players. We map the playing of these phases
--over the list of turn phases, and by folding a GameState through this
--process it is updated through each of the phases in turn.
updateGameState : InputState -> GameState -> GameState
updateGameState input oldGame =
  List.foldl (\turnPhase gameState -> turnPhase input gameState) oldGame turnPhases


type alias TurnPhase =
  InputState -> GameState -> GameState

--First play out movements
--Next play out Iaido attacks
--Next play out standard attacks
--Next award Iaidos
turnPhases : List TurnPhase
turnPhases = [movePhase]
