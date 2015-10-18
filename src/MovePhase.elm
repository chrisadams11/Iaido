
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
      Just playerInputState -> {player = player, inputState = playerInputState}
      Nothing -> Nothing

getPlayerMoveTargets : List PlayerState -> List Tile -> List (Player, Tile)
getPlayerMoveTargets playerStates tiles =
  List.map
    (\ playerState->
      (playerState.player, getPlayerMoveTarget playerState tiles))
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
          (\(previousElem, acc) elem ->
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
