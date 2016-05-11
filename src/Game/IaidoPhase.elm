-------------------------------------------------------------------------------
---------------------------------IAIDO PHASE-----------------------------------
-------------------------------------------------------------------------------
-- This section defines the "Iaido" phase of play, wherein all players
-- who chose to make an Iaido attack execute their attacks. An Iaido attack moves
-- the player two spaces forward, and attacks both of those spaces.
--
-- Players are "immune" while dashing, and thus a player making an Iaido attack
-- cannot, on the same turn, be killed by an Iaido attack. He can, however, be
-- killed by a regular attack in the next phase.
--
-- Similar to the movement phase, two players who would end up on the same tile
-- (by simultaneously dashing to it) are kicked back to their starting tiles.
-- However, their attacks are still executed and can kill players standing in
-- the crossfire.
--
-- Unlike standard movement, Iaido attacks can move through walls.
-- This may change later, depending on how it affects play in testing.
------------------------------------------------------------------------------

-- Allows all of the players in the game to make their Iaido attacks.
-- All players attacks are applied simultaneously, and then their movements
-- are calculated (correcting for collisions).
iaidoPhase : InputState -> GameState -> GameState
iaidoPhase inputState gameState =
  let
    playersWithInputs = (associatePlayersWithInputs inputState.playerInputStates gameState.players)
    playerIAttackTargets = getPlayerIAttackTargets playersWithInputs gameState.board
    playersAfterIAttacks = executeIAttacks playerIAttackTargets
    playerIMoveTargets = getPlayerIMoveTargets playersAfterIAttacks gameState.board
    playersAfterIMovement = iMovePlayers playerIMoveTargets
  in
    {gameState | players = playersIAfterMovement }

-- Finds the desired attack targets of each player.
getPlayerIAttackTargets : List PlayerWithInput -> List Tile -> List (PlayerWithInput, List Tile)
getPlayerIAttackTargets playersWithInputs tiles =
  List.map
    (\ playerWithInput -> (playerWithInput, getPlayerIAttackTarget playerWithInput tiles))
    playersWithInputs


-- Inner loop of the above function,
-- finds the desired attack target for a given player actor.
getPlayerIAttackTarget : PlayerWithInput -> List Tile -> List Tile
getPlayerIAttackTarget playerWithInput tiles =
  if !(playerWithInput.inputState.attack && playerWithInput.player.iaido) then [] else
  let
    firstTile = getTile
                  tiles
                  (playerWithInput.player.tile.position.row + playerWithInput.inputState.moveDIrection.x)
                  (playerWithInput.player.tile.position.col + playerWithInput.inputState.moveDirection.y)
    secondTile = getTile
                  tiles
                  (playerWithInput.player.tile.position.row + (playerWithInput.inputState.moveDIrection.x * 2))
                  (playerWithInput.player.tile.position.col + (playerWithInput.inputState.moveDirection.y * 2))
  in
    firstTile :: secondTile :: []
    |> filterMaybe


-- Executes the players' iaido attacks, inflicting "hit" status on any player hit
-- by another player's attack. As stated above, two players who simultaneously
-- hit each other do not kill each other (to prevent ties).
executeIAttacks : List (PlayerWithInput, List Tile) -> List PlayerWithInput
executeIAttacks playerIAttackTargets =
    let
      playersAfterIAttacks = List.map (executeIAttacksAgainstPlayer playerIAttackTargets) (List.map fst playerIAttackTargets)
    in
      playersAfterIAttacks


-- Inner loop of the above function, checks if a player got "hit" by any attacks
-- and if so, applies the "hit" status.
executeIAttackAgainstPlayer : List (PlayerWithInput, List Tile) -> PlayerWithInput -> PlayerWithInput
executeIAttackAgainstPlayer playerIAttackTargets playerWithInput =
  let
    playerGotHit = !(playerWithInput.inputState.attack && playerWithInput.player.iaido) --Player was not performing an Iaido attack
                    AND List.any (\(attackingPlayer, attackedTiles) =>
                          List.any (\attackedTile =>
                            compareTiles player.tile attackedTile == EQ) attackedTiles)
                          playerIAttackTargets
  in
    if playerGotHit then
      let
        player = playerWithInput.player
      in
        {playerWithInput | player = {player | hit = True}}
    else
      playerWithInput


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
