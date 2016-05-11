type Action = AttackAction | MoveAction | IaidoAction | ReadyAction

type Priority = High | Moderate | Low | Lowest

type Effect = DamageEffect | BlockEffect | MoveEffect | InvulnEffect | VulnEffect

type alias EffectQueue =
  { highStack : List Effect
  , moderateStack : List Effect
  , lowStack : List Effect
  , lowestStack : List Effect
}

type alias AttackAction =
  { playerId : PlayerID
  , direction : Vector
}

type alias MoveAction =
  { playerId : PlayerID
  , direction : Vector
}

type alias IaidoAction =
  { playerId : PlayerID
  , direction : Vector
}

type alias ReadyAction =
  { playerId : PlayerID
}

type alias DamageEffect =
  { sourcePlayer : PlayerID
  , direction : Vector
  , targetTile : Tile
  , priority : Priority
}

type BlockEffect =
  { sourcePlayer : PlayerID
  , direction : Vector
  , targetTile : Tile
  , priority : Priority
}

type MoveEffect =
  { targetPlayer : PlayerID
  , destination : Tile
  , priority : Priority
}

type InvulnEffect =
  { targetPlayer : PlayerID
  , priority : Priority
}

type VulnEffect =
  { targetPlayer : PlayerID
  , priority : Priority
}

resolveEffectQueue : EffectQueue -> GameState -> GameState
resolveEffectQueue effects game = List.foldr resolveEffectStack game [effects.highStack, effects.moderateStack, effects.lowStack, effects.lowestStack]

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
resolveInvuln : InvulnEffect -> (GameState, List Effect)
resolveVuln : VulnEffect -> (GameState, List Effect)

doActions : List Action -> EffectQueue -> EffectQueue
doActions actions game = List.foldr doAction effects actions

doAction : Action -> EffectQueue -> EffectQueue
doAction action effects =
  case action of
    AttackAction attack -> doAttack attack effects
    MoveAction move -> doMove move effects
    IaidoAction iaido -> doIaido iaido effects
    ReadyAction ready -> doReady ready effects

doAttack : AttackAction -> EffectQueue -> EffectQueue
doMove : MoveAction -> EffectQueue -> EffectQueue
doIaido : IaidoAction -> EffectQueue -> EffectQueue
doReady : ReadyAction -> EffectQueue -> EffectQueue
