-------------------------------------------------------------------------------
---------------------------------ATTACK PHASE----------------------------------
-------------------------------------------------------------------------------
-- This section defines the "attaco" phase of play, wherein all players
-- who chose to attack on this turn are execute their attacks, and "hit" players
-- are killed off.
------------------------------------------------------------------------------

-- Allows all of the players in the game to make their attacks.
-- All players attack simultaneously, and all players who are hit are
-- killed off afterward. If two players attack each other at the same time,
-- they collide and neither dies (to prevent tie games).
attackPhase : InputState -> GameState -> GameState
attackPhase inputState gameState =
  let
    playersWithInputs = (associatePlayersWithInputs inputState.playerInputStates gameState.players)
    playerAttackTargets = getPlayerAttackTargets playersWithInputs gameState.board
  in
    {gameState | players = executeAttacks gameState.players playerAttackTargets}


-- Finds the desired attack targets of each player.
getPlayerAttackTargets : List PlayerWithInput -> List Tile -> List (PlayerWithInput, Maybe Tile)
getPlayerAttackTargets playersWithInputs tiles =
  List.map
    (\ playerWithInput -> (playerWithInput, getPlayerAttackTarget playerWithInput tiles))
    playersWithInputs


-- Inner loop of the above function,
-- finds the desired attack target for a given player actor.
getPlayerAttackTarget : PlayerWithInput -> List Tile -> Maybe Tile
getPlayerAttackTarget playerWithInput tiles =
  if !(playerWithInput.inputState.attack && !playerWithInput.player.iaido) then Nothing else
  let
    targetRow = (playerWithInput.player.tile.position.row + playerWithInput.inputState.moveDirection.x)
    targetCol = (playerWithInput.player.tile.position.col + playerWithInput.inputState.moveDirection.y)
    targetTile = getTile tiles targetRow targetCol
  in
    case targetTile of
      Just tile -> tile
      Nothing   -> Nothing


-- Executes the players attacks, inflicting "hit" status on any player hit
-- by another player's attack. As stated above, two players who simultaneously
-- hit each other do not kill each other (to prevent ties).
executeAttacks : List Player -> List (PlayerWithInput, Tile) -> List Player
executeAttacks players playerAttackTargets =
  let
    unblockedAttackTargets = removeBlockedAttacks playerAttackTargets
  in
    List.foldr executeAttack players unblockedAttackTargets


-- Inner loop of the above function, "hits" any player standing on an attacked tile.
executeAttack : (PlayerWithInput, Tile) -> List Player -> List Player
executeAttack (playerWithInput, attackTarget) players =
  let
    targetPlayer = List.filter
                    (\player => (compareTiles player.tile attackTarget) == EQ)
                    players
                  |> List.head
  in
    case targetPlayer of
      Nothing ->
        players

      Just hitPlayer ->
        {hitPlayer | status = Hit} ::
        (List.filter
          (\player => player.playerID != hitPlayer.playerID)
          players)


-- Removes attacks from a list that would cause mutual death, to prevent ties.
removeBlockedAttacks : List (PlayerWithInput, Tile)
removeBlockedAttacks playersAttackTargets =
  List.foldr removeAttackIfBlocked playersAttackTargets playersAttackTargets


-- Inner loop of the above function,
-- Removes an attack (and all blocking attacks) from the list if it is blocked.
removeAttackIfBlocked : (PlayerWithInput, Tile) -> List (PlayerWithInput, Tile) -> List (PlayerWithInput, Tile)
removeAttackIfBlocked (attackingPlayer, attackTile) attacks =
  let
    blockingAttacks = List.filter
      (\(blockingPlayer, blockTile) =>
        ((compareTiles blockingPlayer.tile attackTile) == EQ) &&
        ((compareTiles attackingPlayer.tile blockTile) == EQ))
      attacks
    blockingPlayerIds = List.map (\(blockingPlayer, _) => blockingPlayer.playerId) blockingAttacks
    isBlocked = (blockingAttacks == [])
  in
    if not isBlocked then attacks else
      attacks
      |> List.filter
          (\(player, _) =>
            not (List.member player.playerId blockingPlayerIds) &&
            not player.playerId == attackingPlayer.playerId)
