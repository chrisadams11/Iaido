-------------------------------------------------------------------------------
---------------------------------GAMESTATE-------------------------------------
-------------------------------------------------------------------------------
-- This section defines the GameState structure, which describes the state of
-- a single frame of battle-state gameplay.
------------------------------------------------------------------------------

--FUNCTION: updateGameState
--DESCRIPTION:
--  Updates the game state given the input.
--  Each frame consists of an indeterminate number of "phases."
--  Each phase is the mapping of a particular function with type TurnPhase
--  over the collection of players. We map the playing of these phases
--  over the list of turn phases, and by folding a GameState through this
--  process it is updated through each of the phases in turn.
--PARAMETERS:
-- input : InputState
--  The state of all players' inputs
-- oldGame : GameState
--  The state of the previous frame of gameplay
--RETURNS: The frame of gameplay that follows the one given, with the given inputs.
updateGameState : InputState -> GameState -> GameState
updateGameState input oldGame =
  List.foldl (\turnPhase gameState -> turnPhase input gameState) oldGame turnPhases
