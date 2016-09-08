module Update exposing (..)

import Model exposing (..)
import Model.InputModels exposing (..)
import Update.InputLogic exposing (..)
import Update.GameLogic exposing (..)
import Update.ViewLogic exposing (..)
import Time exposing (..)

type Msg
    = TitleBegin
    | Tick Time


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Title ->
            updateTitle msg

        Game gameModel ->
            updateGame msg gameModel


updateTitle : Msg -> ( Model, Cmd msg )
updateTitle msg =
    case msg of
        TitleBegin ->
            ( initGameModel, Cmd.none )

        _ ->
            ( Title, Cmd.none )


updateGame : Msg -> GameModel -> ( Model, Cmd msg )
updateGame msg game =
    case msg of
        -- Main game loop
        Tick time ->
            let
                updatedInputState =
                    updateInputState game.inputFrame game.inputState

                updatedGameState =
                    updateGameState updatedInputState game.gameState

                updatedDrawState =
                    updateDrawState game.gameState game.inputState game.drawState
            in
                ( Game
                    { game
                        | inputFrame = newInputFrame
                        , inputState = updatedInputState
                        , gameState = updatedGameState
                        , drawState = updatedDrawState
                    }
                , Cmd.none
                )

        -- Input messages

        _ ->
            ( Game game, Cmd.none )