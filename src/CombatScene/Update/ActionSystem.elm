module CombatScene.Update.ActionSystem exposing (..)

import Utility exposing (..)
import CombatScene.Model.GameModels exposing (..)
import CombatScene.Update.GameUtilities exposing (..)


type Action
    = Action_Attack AttackAction
    | Action_Move MoveAction
    | Action_Iaido IaidoAction
    | Action_Ready ReadyAction


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


type Effect
    = Effect_Damage DamageEffect
    | Effect_Block BlockEffect
    | Effect_Move MoveEffect
    | Effect_Status StatusEffect


type alias DamageEffect =
    { sourcePlayerID : PlayerID
    , direction : Vector
    , targetTile : Tile
    }


type alias BlockEffect =
    { sourcePlayerID : PlayerID
    , direction : Vector
    , targetTile : Tile
    }


type alias MoveEffect =
    { targetPlayerID : PlayerID
    , destination : Tile
    }


type alias StatusEffect =
    { targetPlayerID : PlayerID
    , status : Status
    , value : Bool
    }


type alias EffectQueue =
    { highStack : List Effect
    , midStack : List Effect
    , lowStack : List Effect
    , lowestStack : List Effect
    }


resolveEffectQueue : EffectQueue -> GameState -> GameState
resolveEffectQueue effects game =
    List.foldr resolveEffectStack game [ effects.highStack, effects.midStack, effects.lowStack, effects.lowestStack ]


resolveEffectStack : List Effect -> GameState -> GameState
resolveEffectStack effects game =
    List.foldr (resolveEffect effects) game effects


resolveEffect : List Effect -> Effect -> GameState -> GameState
resolveEffect effects effect game =
    case effect of
        Effect_Damage damage ->
            resolveDamage damage effects game

        Effect_Block block ->
            resolveBlock block effects game

        Effect_Move move ->
            resolveMove move effects game

        Effect_Status status ->
            resolveStatus status effects game


resolveDamage : DamageEffect -> List Effect -> GameState -> GameState
resolveDamage damage effects game =
    let
        damageBlocked =
            List.any
                (\e ->
                    case e of
                        Effect_Block b ->
                            (compareTiles b.targetTile damage.targetTile == EQ) && b.direction == damage.direction

                        _ ->
                            False
                )
                effects
    in
        if damageBlocked then
            game
        else
            let
                attackTargets =
                    List.filter (\player -> compareTiles (unsafe <| getTile game.board player.position) damage.targetTile == EQ) game.players
            in
                case List.head attackTargets of
                    Nothing ->
                        game

                    Just player ->
                        let
                            updatedPlayer =
                                { player | hit = True }
                        in
                            { game | players = updatePlayer game.players updatedPlayer }


resolveBlock : BlockEffect -> List Effect -> GameState -> GameState
resolveBlock block effects game =
    game


resolveMove : MoveEffect -> List Effect -> GameState -> GameState
resolveMove move effects game =
    let
        immobilePlayers =
            List.filter
                (\player ->
                    not
                        (List.any
                            (\effect ->
                                case effect of
                                    Effect_Move m ->
                                        m.targetPlayerID == player.playerID

                                    _ ->
                                        False
                            )
                            effects
                        )
                )
                game.players

        blockingPlayers =
            List.filter (\player -> compareTiles (unsafe <| getTile game.board player.position) move.destination == EQ) immobilePlayers

        blockingMovements =
            List.filter
                (\e ->
                    case e of
                        Effect_Move m ->
                            (m.destination == move.destination) && (m.targetPlayerID /= move.targetPlayerID)

                        _ ->
                            False
                )
                effects
    in
        if blockingPlayers /= [] || blockingMovements /= [] then
            game
        else
            let
                movingPlayer =
                    unsafe (getPlayer game move.targetPlayerID)

                updatedPlayer =
                    { movingPlayer | position = move.destination.position }
            in
                { game | players = updatePlayer game.players updatedPlayer }


resolveStatus : StatusEffect -> List Effect -> GameState -> GameState
resolveStatus status effects game =
    let
        affectedPlayer =
            unsafe (getPlayer game status.targetPlayerID)

        playerHasStatus =
            listContains affectedPlayer.status status.status

        updatedPlayer =
            { affectedPlayer
                | status =
                    if playerHasStatus == status.value then
                        affectedPlayer.status
                    else if status.value == True then
                        status.status :: affectedPlayer.status
                    else
                        listRemove affectedPlayer.status status.status
            }
    in
        { game | players = updatePlayer game.players updatedPlayer }


doActions : List Action -> GameState -> EffectQueue
doActions actions game =
    List.foldr (doAction game) (EffectQueue [] [] [] []) actions


doAction : GameState -> Action -> EffectQueue -> EffectQueue
doAction game action effects =
    case action of
        Action_Attack attack ->
            doAttack game attack effects

        Action_Move move ->
            doMove game move effects

        Action_Iaido iaido ->
            doIaido game iaido effects

        Action_Ready ready ->
            doReady game ready effects


doAttack : GameState -> AttackAction -> EffectQueue -> EffectQueue
doAttack game attackAction effectQueue =
    case calculateAttackTarget game attackAction.player attackAction.direction of
        Nothing ->
            effectQueue

        Just tile ->
            let
                damageEffect =
                    { sourcePlayerID = attackAction.player.playerID, direction = attackAction.direction, targetTile = tile }

                blockEffect =
                    { sourcePlayerID = attackAction.player.playerID, direction = reverseVector attackAction.direction, targetTile = unsafe <| getTile game.board attackAction.player.position }
            in
                { effectQueue | lowStack = Effect_Damage damageEffect :: Effect_Block blockEffect :: effectQueue.lowStack }


calculateAttackTarget : GameState -> Player -> Vector -> Maybe Tile
calculateAttackTarget game player dir =
    let
        targetX =
            (player.position.x + dir.x)

        targetY =
            (player.position.y + dir.y)

        targetTile =
            getTile game.board { x = targetX, y = targetY }
    in
        targetTile


doMove : GameState -> MoveAction -> EffectQueue -> EffectQueue
doMove game moveAction effectQueue =
    case calculateMoveTarget game moveAction.player moveAction.direction of
        Nothing ->
            effectQueue

        Just tile ->
            let
                moveEffect =
                    { targetPlayerID = moveAction.player.playerID, destination = tile }
            in
                { effectQueue | highStack = Effect_Move moveEffect :: effectQueue.highStack }


calculateMoveTarget : GameState -> Player -> Vector -> Maybe Tile
calculateMoveTarget game player dir =
    let
        targetX =
            (player.position.x + dir.x)

        targetY =
            (player.position.y + dir.y)

        targetTile =
            getTile game.board { x = targetX, y = targetY }
    in
        targetTile


doIaido : GameState -> IaidoAction -> EffectQueue -> EffectQueue
doIaido game iaidoAction effectQueue =
    let
        attackTarget1 =
            calculateAttackTarget game iaidoAction.player iaidoAction.direction

        attackTarget2 =
            calculateAttackTarget game iaidoAction.player (scaleVector iaidoAction.direction 2)

        moveTarget =
            calculateMoveTarget game iaidoAction.player (scaleVector iaidoAction.direction 2)

        damageEffect1 =
            case attackTarget1 of
                Nothing ->
                    Nothing

                Just target ->
                    Just { sourcePlayerID = iaidoAction.player.playerID, direction = iaidoAction.direction, targetTile = target }

        damageEffect2 =
            case attackTarget2 of
                Nothing ->
                    Nothing

                Just target ->
                    Just { sourcePlayerID = iaidoAction.player.playerID, direction = iaidoAction.direction, targetTile = target }

        moveEffect =
            case moveTarget of
                Nothing ->
                    Nothing

                Just target ->
                    Just { targetPlayerID = iaidoAction.player.playerID, destination = target }

        invulnEffect =
            { targetPlayerID = iaidoAction.player.playerID, status = Status_Invulnerability, value = True }

        endInvulnEffect =
            { targetPlayerID = iaidoAction.player.playerID, status = Status_Invulnerability, value = False }

        endReadyEffect =
            { targetPlayerID = iaidoAction.player.playerID, status = Status_Ready, value = False }
    in
        { effectQueue
            | highStack = Effect_Status invulnEffect :: effectQueue.highStack
            , midStack =
                List.append
                    (filterMaybe [ Maybe.map Effect_Damage damageEffect1, Maybe.map Effect_Damage damageEffect2, Maybe.map Effect_Move moveEffect ])
                    effectQueue.midStack
            , lowestStack = Effect_Status endInvulnEffect :: Effect_Status endReadyEffect :: effectQueue.lowestStack
        }


doReady : GameState -> ReadyAction -> EffectQueue -> EffectQueue
doReady game readyAction effectQueue =
    let
        readyEffect =
            { targetPlayerID = readyAction.player.playerID, status = Status_Ready, value = True }
    in
        { effectQueue | lowestStack = Effect_Status readyEffect :: effectQueue.lowestStack }
