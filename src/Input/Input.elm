-------------------------------------------------------------------------------
--------------------------------INPUT------------------------------------------
--The input section encapsulates all functions related to retrieving input    -
--from outside sources, including hardware events, network signals, and time  -
-------------------------------------------------------------------------------

--the time between frames, as close to 30 fps as
--the browser can render
delta =
  Signal.map inSeconds (fps 30)


--Updates an input state given a new frame of input data
updateInputState: InputFrame -> InputState -> InputState
updateInputState inputFrame oldState =
  { playerInputStates =
      List.map2 updatePlayerInputState inputFrame.playerInputs oldState.playerInputStates
  , delta = inputFrame.delta
  }


--Updates a player input state given a new frame of player input data
updatePlayerInputState: PlayerInputFrame -> PlayerInputState -> PlayerInputState
updatePlayerInputState playerInputFrame oldState =
  { playerID = oldState.playerID
  , moveDirection = playerInputFrame.moveKeys
  , attack = playerInputFrame.attackBtn
  }


--Constructs a frame of player input data from raw key data
playerInputFrame: PlayerID -> Bool -> Bool -> Bool -> Bool -> Bool -> PlayerInputFrame
playerInputFrame playerID up down left right attackBtn =
  let
    x =
      if right then
        1
      else if left then
        -1
      else
        0
    y =
      if up then
        1
      else if down then
        -1
      else
        0
    moveKeys =
      { x = x
      , y = y
      }
  in
    { playerID = playerID
    , moveKeys = moveKeys
    , attackBtn = attackBtn
    }


--Processes a frame of all raw input data into a frame of player inputs
processFrameInputs: List InputSource -> Set KeyCode -> List PlayerInputFrame
processFrameInputs inputSources keys  =
  List.map (processFrameInput keys) inputSources


--Processes a frame of a specific player's raw input
--into a frame of player input
processFrameInput: Set KeyCode -> InputSource -> PlayerInputFrame
processFrameInput keys inputSource =
  playerInputFrame
    inputSource.playerID
    (Set.member inputSource.upKey keys)
    (Set.member inputSource.downKey keys)
    (Set.member inputSource.leftKey keys)
    (Set.member inputSource.rightKey keys)
    (Set.member inputSource.attackKey keys)


--Once per frame,
--grab all of the raw input data from the input sources
--and process them into a frame of game input.
inputFrameStream : Signal InputFrame
inputFrameStream =
  Signal.sampleOn delta
  <| (Signal.map2 InputFrame (Signal.map (processFrameInputs inputSources) Keyboard.keysDown) delta)


--Stateful processing of input data
inputStateStream : Signal InputState
inputStateStream =
  Signal.foldp updateInputState startingInputState inputFrameStream
