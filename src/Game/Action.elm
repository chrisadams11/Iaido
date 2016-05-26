type Action = AttackAction | MoveAction | IaidoAction | ReadyAction

type Effect = DamageEffect | BlockEffect | MoveEffect | StatusEffect

type Status = Ready | Invulnerability | Stun

type alias EffectQueue =
  { highStack : List Effect
  , midStack : List Effect
  , lowStack : List Effect
  , lowestStack : List Effect
}

type alias AttackAction =
  { player : Player
  , direction : Vector
}

type alias MoveAction =
  { player : Player
  , direction : Vector
}

type alias IaidoAction =
  { player : Player
  , direction : Vector
}

type alias ReadyAction =
  { player : Player
}

type alias DamageEffect =
  { sourcePlayer : Player
  , direction : Vector
  , targetTile : Tile
}

type BlockEffect =
  { sourcePlayer : Player
  , direction : Vector
  , targetTile : Tile
}

type MoveEffect =
  { targetPlayer : Player
  , destination : Tile
}

type StatusEffect =
  { targetPlayer : Player
  , status : Status
  , value : Bool
}

resolveEffectQueue : EffectQueue -> GameState -> GameState
resolveEffectQueue effects game = List.foldr resolveEffectStack game [effects.highStack, effects.midStack, effects.lowStack, effects.lowestStack]

resolveEffectStack : List Effect -> GameState -> GameState
resolveEffectStack effects game = list.foldr resolveEffect (game, effects) effects

resolveEffect : Effect -> (GameState, List Effect) -> (GameState, List Effect)
resolveEffect effect (game, effects) =
  case effect of
    DamageEffect damage -> resolveDamage damage (game, effects)
    BlockEffect block -> resolveBlock block (game, effects)
    MoveEffect move -> resolveMove move (game, effects)
    InvulnEffect invuln -> resolveInvuln invuln (game, effects)
    VulnEffect vuln -> resolveVuln vuln (game, effects)

resolveDamage : DamageEffect -> (GameState, List Effect)
resolveBlock : BlockEffect -> (GameState, List Effect)
resolveMove : MoveEffect -> (GameState, List Effect)
resolveStatus : StatusEffect -> (GameState, List Effect)

doActions : GameState -> List Action -> EffectQueue -> EffectQueue
doActions game actions effects = List.foldr (doAction game) effects actions

doAction : GameState -> Action -> EffectQueue -> EffectQueue
doAction game action effects =
  case action of
    AttackAction attack -> doAttack game attack effects
    MoveAction move -> doMove game move effects
    IaidoAction iaido -> doIaido game iaido effects
    ReadyAction ready -> doReady game ready effects

doAttack : GameState -> AttackAction -> EffectQueue -> EffectQueue
doAttack game attackAction effectQueue =
  let
    attackTarget = calculateAttackTarget game attackAction.player attackAction.direction
    damageEffect = { sourcePlayer = attackAction.player, direction = attackAction.direction, targetTile = attackTarget }
    blockEffect = { sourcePlayer = attackAction.player, direction = reverseVector attackAction.direction, targetTile = attackTarget }
  in
    {effectQueue | lowStack = damageEffect :: blockEffect :: effectQueue.lowStack}

calculateAttackTarget : GameState -> Player -> Vector -> Tile
calculateAttackTarget game player dir =
  let
    targetRow = (player.tile.position.row + dir.x)
    targetCol = (player.tile.position.col + dir.y)
  in
    getTile game.board targetRow targetCol

doMove : GameState -> MoveAction -> EffectQueue -> EffectQueue
doMove game moveAction effectQueue =
  let
    moveTarget = calculateMoveTarget game moveAction.player moveAction.direction
    moveEffect = { targetPlayer = moveAction.playerId, destination = moveTarget }
  in
    {effectQueue | highStack = moveEffect :: effectQueue.highStack}

calculateMoveTarget : GameState -> Player -> Vector -> Tile
calculateMoveTarget game player dir =
  let
    targetRow = (player.tile.position.row + dir.x)
    targetCol = (player.tile.position.col + dir.y)
  in
    getTile game.board targetRow targetCol


doIaido : GameState -> IaidoAction -> EffectQueue -> EffectQueue
doIaido game iaidoAction effectQueue =
  let
    attackTarget1 = calculateAttackTarget game player dir
    attackTarget2 = calculateAttackTarget game player (dir * 2)
    moveTarget = calculateMoveTarget game iaidoAction.player (iaidoAction.direction * 2)
    damageEffect1 = { sourcePlayer = iaidoAction.player, direction = iaidoAction.direction, targetTile = attackTarget1 }
    damageEffect2 = { sourcePlayer = iaidoAction.player, direction = iaidoAction.direction, targetTile = attackTarget2 }
    moveEffect = {targetPlayer = moveAction.playerId, destination = moveTarget }
    invulnEffect = { targetPlayer = iaidoAction.player, status = Invulnerability, value = True }
    endInvulnEffect = { targetPlayer = iaidoAction.player, status = Invulnerability, value = False }
    endReadyEffect = { targetPlayer = iaidoAction.player, status = Ready, value = False }
  in
    {effectQueue |
      highStack = invulnEffect :: effectQueue.highStack,
      midStack = damageEffect1 :: damageEffect2 :: moveEffect :: effectQueue.midStack,
      lowestStack = endInvulnEffect :: endReadyEffect :: effectQueue.lowestStack}

doReady : GameState -> ReadyAction -> EffectQueue -> EffectQueue
doReady game readyAction effectQueue =
  let
    readyEffect = { targetPlayer = readyAction.player, status = Ready, value = True }
  in
    {effectQueue | lowestStack = readyEffect :: effectQueue.lowestStack}
