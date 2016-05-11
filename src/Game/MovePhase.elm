-------------------------------------------------------------------------------
---------------------------------MOVE PHASE------------------------------------
-------------------------------------------------------------------------------
-- This section defines the "Movement" phase of play, wherein all players
-- who chose to move on this turn are moved to their destination tiles.
------------------------------------------------------------------------------

-- Allows all of the players in the game to make their movements.
-- All players move simultaneously, and players who collide with each other
-- or the wall are "blocked" and stay on their original tile.
movePhase : InputState -> GameState -> GameState
movePhase inputState gameState =
  let
    playersWithInputs = (associatePlayersWithInputs inputState.playerInputStates gameState.players)
    playerMoveTargets = getPlayerMoveTargets playersWithInputs gameState.board
  in
    {gameState | players = movePlayers playerMoveTargets }


-- Finds the desired movement targets of each player. These targets may be blocked,
-- but that will be resolved later.
getPlayerMoveTargets : List PlayerWithInput -> List Tile -> List (PlayerWithInput, Tile)
getPlayerMoveTargets playersWithInputs tiles =
  List.map
    (\ playerWithInput -> (playerWithInput, getPlayerMoveTarget playerWithInput tiles))
    playersWithInputs


-- Inner loop of the above function, finds the desired movement target for a player actor.
getPlayerMoveTarget : PlayerWithInput -> List Tile -> Tile
getPlayerMoveTarget playerWithInput tiles =
  let
    targetRow = (playerWithInput.player.tile.position.row + if not playerWithInput.inputState.attack then playerWithInput.inputState.moveDirection.x else 0)
    targetCol = (playerWithInput.player.tile.position.col + if not playerWithInput.inputState.attack then playerWithInput.inputState.moveDirection.y else 0)
    targetTile = getTile tiles targetRow targetCol
  in
    case targetTile of
      Just tile -> tile
      Nothing   -> playerWithInput.player.tile


-- Moves the players around the board by attempting to place each player
-- on their desired nearby tile.
movePlayers : List (PlayerWithInput, Tile) -> List Player
movePlayers moveTargets =
  let
    correctedMoveTargets = resolveCollisions moveTargets
  in
    List.map movePlayer correctedMoveTargets

-- Recursively force colliding players back to their starting square until
-- no more players are colliding. Recursion is necessary because sometimes, forcing
-- a player back to their starting tile will cause them to collide with another player.
resolveCollisions : List (PlayerWithInput, Tile) -> List (PlayerWithInput, Tile)
resolveCollisions moveTargets =
  let
    crowdedTiles = findRepeatTiles (List.map snd moveTargets)
  in
    if crowdedTiles == [] then moveTargets else
      let
        newMoveTargets =
          List.foldr
            (\({player, inputState}, targetTile) acc ->
              if List.member targetTile crowdedTiles
                then (PlayerWithInput {player | collided = True} inputState, player.tile) :: acc -- Keep the player on their current tile
                else (PlayerWithInput player inputState, targetTile) :: acc) -- Let them move... For now
            [] -- Accumulator is the new list of move targets
            moveTargets
      in
        resolveCollisions newMoveTargets -- Recurse


-- Inner loop of the above function, moves a player to their desired nearby tile
-- if no other player is "crowding" that tile (staying on it or attempting to move into it)
-- and the tile is not walled off from their direction.
movePlayer : (PlayerWithInput, Tile) -> Player
movePlayer (playerWithInput, moveTarget) =
  let
    player = playerWithInput.player
    inputState = playerWithInput.inputState
  in
    if
      (isWalled player.tile inputState.moveDirection) -- Am I walled inside my current tile
      || (isWalled moveTarget {x = inputState.moveDirection.x * -1, y = inputState.moveDirection.y * -1}) -- Am I walled from my target tile
    then {player | collided = True}
    else {player | tile = moveTarget}
