module Game where

import Set exposing (Set)
import Signal exposing (Signal)
import Signal
import Time exposing (..)
import Char
import Maybe
import Maybe exposing (withDefault)
import List
import List exposing  ((::))
import Debug
import Input exposing (..)
import Globals exposing (..)

--The battle game state, consisting of all the game board and all of the players
type alias GameState =
  { board:List Tile
  , players:List Player
  }


type PlayerStatus = OK | Hit


--Definition of the Player actor.
type alias Player =
  { playerID: PlayerID
  , tile: Tile
  , iaido: Bool
  , collided: Bool
  , status: PlayerStatus
  , momentum:
      { dx: Int
      , dy: Int
      }
  }


--Template for a TurnPhase function. Each turn phase takes the input state
--and the previous game state, and returns a new post-phase state of the game
type alias TurnPhase =
  InputState -> GameState -> GameState


--Utility structure that associates a player actor with their corresponding
--player input. Easier than moving them around together in tuples.
type alias PlayerWithInput =
  { player: Player
  , inputState: PlayerInputState
  }


--First play out movements
--Next play out Iaido attacks
--Next play out standard attacks
--Next award Iaidos
turnPhases : List TurnPhase
turnPhases = [movePhase]


-- Utility function that associates player actors with their corresponding inputs.
associatePlayersWithInputs : List PlayerInputState -> List Player -> List PlayerWithInput
associatePlayersWithInputs playerInputStates players =
    List.filterMap (\player -> associatePlayerWithInput playerInputStates player) players


--Inner loop of the above function, associates a player actor with its input from a list.
associatePlayerWithInput : List PlayerInputState -> Player -> Maybe PlayerWithInput
associatePlayerWithInput playerInputStates player =
  let
    foundInputState = List.head
                        (List.filter
                          (\inputState -> inputState.playerID == player.playerID)
                          playerInputStates)
  in
    case foundInputState of
      Just playerInputState ->
        Just (PlayerWithInput player playerInputState)

      Nothing ->
        Nothing
