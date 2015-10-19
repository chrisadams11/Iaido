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
---------------------------------TILE------------------------------------------
-------------------------------------------------------------------------------

type alias Tile =
  {
    position: {row: Int, col: Int},
    walls:
      {
        left: Bool,
        right: Bool,
        top: Bool,
        bottom: Bool
      }
  }

getTile : List Tile -> Int -> Int -> Maybe Tile
getTile tiles row col =
  List.filter (\tile -> tile.position.row == row && tile.position.col == col) tiles
  |> List.head

isWalled : Tile -> {x: Int, y: Int} -> Bool
isWalled tile {x, y} =
  let
    isBlockedHorizontally = ((x == 1) && tile.walls.right) || ((x == -1) && tile.walls.left)
    isBlockedVertically = ((y == 1) && tile.walls.top) || ((y == -1) && tile.walls.bottom)
  in
    isBlockedVertically && isBlockedHorizontally

type alias PlayerState =
  {
    player: Player,
    inputState: PlayerInputState
  }

movePhase : InputState -> GameState -> GameState
movePhase inputState gameState =
  let
    playerStates = (associatePlayerInputs gameState.players inputState.playerInputStates)

    moveTargets: List (PlayerState, Tile)
    moveTargets = getPlayerMoveTargets playerStates gameState.board
  in
    {gameState | players <- movePlayers moveTargets }

associatePlayerInputs : List Player -> List PlayerInputState -> List PlayerState
associatePlayerInputs players playerInputStates =
    List.filterMap (\player -> associatePlayerInput playerInputStates player) players

associatePlayerInput : List PlayerInputState -> Player -> Maybe PlayerState
associatePlayerInput playerInputStates player =
  let
    foundInputState = List.head
                        (List.filter
                          (\inputState -> inputState.playerID == player.playerID)
                          playerInputStates)
  in
    case foundInputState of
      Just playerInputState -> Just {player = player, inputState = playerInputState}
      Nothing -> Nothing

getPlayerMoveTargets : List PlayerState -> List Tile -> List (PlayerState, Tile)
getPlayerMoveTargets playerStates tiles =
  List.map
    (\ playerState-> (playerState, getPlayerMoveTarget playerState tiles))
    playerStates

getPlayerMoveTarget : PlayerState -> List Tile -> Tile
getPlayerMoveTarget playerState tiles =
  let
    targetRow = (playerState.player.tile.position.row + playerState.inputState.moveDirection.x)
    targetCol = (playerState.player.tile.position.col + playerState.inputState.moveDirection.y)
    targetTile = getTile tiles targetRow targetCol
  in
    case targetTile of
      Just tile -> tile
      Nothing   -> playerState.player.tile

movePlayers : List (PlayerState, Tile) -> List Player
movePlayers moveTargets =
  let
    crowdedTiles = findRepeatTiles (List.map snd moveTargets)
  in
    List.map (movePlayer crowdedTiles) moveTargets

movePlayer : List Tile -> (PlayerState, Tile) -> Player
movePlayer crowdedTiles (playerState, moveTarget) =
  let
    player = playerState.player
    inputState = playerState.inputState
  in
    if
      (List.member moveTarget crowdedTiles)
      || (isWalled player.tile inputState.moveDirection)
      || (isWalled moveTarget {x = inputState.moveDirection.x * -1, y = inputState.moveDirection.y * -1})
    then {player | collided <- True}
    else {player | tile <- moveTarget}

findRepeatTiles: List Tile -> List Tile
findRepeatTiles l =
  if l == [] then []
  else
    let
      sortedList = List.sortWith compareTiles l
    in
      snd
        (List.foldl
          (\elem (previousElem, acc) ->
            if (elem == previousElem)
            then (elem, elem :: acc)
            else (elem, acc))
          (unsafeHead sortedList, [])
          (unsafeTail sortedList))

compareTiles: Tile -> Tile -> Order
compareTiles l r =
  if l.position.row > r.position.row then GT
  else
    if l.position.col > r.position.col then GT
    else
      EQ

unsafeHead : List a -> a
unsafeHead list = unsafe (List.head list)

unsafeTail : List a -> List a
unsafeTail list = unsafe (List.tail list)

unsafe : Maybe a -> a
unsafe x = case x of
  Just safe -> safe
  Nothing   -> Debug.crash "unsafe operation failed!"
