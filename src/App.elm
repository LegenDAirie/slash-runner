port module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import AnimationFrame
import Window
import Task
import GameTypes exposing (Vector)
import Controller exposing (ButtonState(..), calculateButtonState, DPad(..), ControllerState, calculateControllerStateFromKeyboardState, initialControllerState, calculateControllerStateFromGamePad)
import Coordinates exposing (gameSize, convertTouchCoorToGameCoor, convertToGameUnits, pixelToGridConversion, gridToPixelConversion)
import Screens.NormalPlay exposing (initialNormalPlayState, LevelData, createLevel, updateNormalPlay, renderNormalPlay, NormalPlayState, jsonToLevelData)
import Keyboard.Extra
import Json.Decode exposing (Decoder)
import CustomEncoders exposing (encodeVector, levelDataEncodeHandler)
import Mouse
import GamePlatform exposing (Platform, platformSize)
import Enemy exposing (Enemy)
import MouseHelpers exposing (mouseToGridInPixels)


type alias Model =
    { canvasSize : Vector
    , keyboardState : Keyboard.Extra.State
    , controllerState : ControllerState
    , gameScreen : GameScreen
    }


type GameScreen
    = Uninitialized
    | NormalPlay NormalPlayState


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | GetControllerState
    | Tick { up : Bool, left : Bool, right : Bool, down : Bool, jump : Bool, dash : Bool }
    | Resources Resources.Msg
    | KeyboardMsg Keyboard.Extra.Msg
    | ReceiveLevelData LevelData
    | MouseMove Vector
    | MouseClick Vector


initialModel : Model
initialModel =
    { canvasSize = ( 0, 0 )
    , keyboardState = Keyboard.Extra.initialState
    , controllerState = initialControllerState
    , gameScreen = NormalPlay initialNormalPlayState
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetCanvasSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/background-square.jpg" ]
          , fetchLevelData 1
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetCanvasSize size ->
            { model | canvasSize = setCanvasSize size }
                ! []

        KeyboardMsg keyMsg ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay normalPlayState ->
                    let
                        pressedKeys =
                            Keyboard.Extra.pressedDown model.keyboardState

                        newEnemies =
                            if List.member Keyboard.Extra.CharR pressedKeys then
                                normalPlayState.permanentEnemies
                            else
                                normalPlayState.enemies

                        newNormalPlayState =
                            { normalPlayState
                                | enemies = newEnemies
                            }
                    in
                        { model
                            | keyboardState = Keyboard.Extra.update keyMsg model.keyboardState
                            , gameScreen = NormalPlay newNormalPlayState
                        }
                            ! []

        Resources msg ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    { model
                        | gameScreen =
                            NormalPlay { state | resources = Resources.update msg state.resources }
                    }
                        ! []

        ReceiveLevelData levelData ->
            -- let
            --     _ =
            --         Debug.log "level data" levelData
            -- in
            { model
                | gameScreen = NormalPlay (createLevel levelData)
            }
                ! []

        MouseMove mousePosition ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    let
                        ( width, height ) =
                            platformSize

                        newPosition =
                            mouseToGridInPixels model.canvasSize state.camera mousePosition

                        newState =
                            { state
                                | mouse = newPosition
                            }
                    in
                        { model
                            | gameScreen = NormalPlay newState
                        }
                            ! []

        MouseClick mousePosition ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    let
                        ( width, height ) =
                            platformSize

                        newPosition =
                            mouseToGridInPixels model.canvasSize state.camera mousePosition

                        newPlatform =
                            Platform newPosition

                        pressedKeys =
                            Keyboard.Extra.pressedDown model.keyboardState

                        newPlatforms =
                            if List.member Keyboard.Extra.Shift pressedKeys then
                                state.platforms
                            else if List.member Keyboard.Extra.CharH pressedKeys && List.member Keyboard.Extra.CharG pressedKeys then
                                []
                            else if List.member newPlatform state.platforms then
                                List.filter (\platform -> not (platform == newPlatform)) state.platforms
                            else
                                [ Platform newPosition ]
                                    |> List.append state.platforms

                        newEnemy =
                            Enemy newPosition 0 ( 64, 64 )

                        newEnemies =
                            if List.member Keyboard.Extra.Shift pressedKeys then
                                if List.member newEnemy.location (List.map (\enemy -> enemy.location) state.enemies) then
                                    List.filter (\enemy -> not (enemy.location == newEnemy.location)) state.enemies
                                else
                                    [ newEnemy ]
                                        |> List.append state.enemies
                            else if List.member Keyboard.Extra.CharH pressedKeys && List.member Keyboard.Extra.CharG pressedKeys then
                                []
                            else
                                state.enemies

                        keyboardState =
                            model.keyboardState

                        newState =
                            { state
                                | platforms = newPlatforms
                                , enemies = newEnemies
                            }

                        encodedLevelData =
                            levelDataEncodeHandler newState.platforms newState.enemies

                        -- _ =
                        --     Debug.log "encoded level data" encodedLevelData
                    in
                        { model
                            | gameScreen = NormalPlay newState
                        }
                            ! [ writeLevelData encodedLevelData ]

        GetControllerState ->
            let
                player1 =
                    1
            in
                model ! [ getControllerState player1 ]

        Tick gamePad ->
            let
                newControllerState =
                    model.controllerState
                        -- |> calculateControllerStateFromGamePad gamePad
                        |>
                            calculateControllerStateFromKeyboardState model.keyboardState

                newModel =
                    { model
                        | controllerState = newControllerState
                    }

                -- _ =
                --     Debug.log "buttons" buttons
            in
                case newModel.gameScreen of
                    Uninitialized ->
                        newModel ! []

                    NormalPlay state ->
                        { newModel
                            | gameScreen =
                                NormalPlay <|
                                    updateNormalPlay
                                        newControllerState
                                        state
                        }
                            ! []


view : Model -> Html Msg
view model =
    let
        ( camera, gameScene ) =
            case model.gameScreen of
                Uninitialized ->
                    ( Camera.fixedWidth (getX gameSize) ( 0, 0 ), [] )

                NormalPlay state ->
                    ( state.camera, renderNormalPlay state )
    in
        div []
            [ Game.renderCenteredWithOptions
                []
                [ style [ ( "border", "solid 1px black" ) ] ]
                { time = 0
                , size = ( floor <| getX model.canvasSize, floor <| getY model.canvasSize )
                , camera = camera
                }
                gameScene
            ]


setCanvasSize : Window.Size -> ( Float, Float )
setCanvasSize size =
    let
        width =
            min size.width <|
                floor (16 / 9 * toFloat size.height)

        height =
            min size.height <|
                floor (9 / 16 * toFloat size.width)
    in
        ( toFloat width, toFloat height )


port fetchLevelData : Int -> Cmd msg


port receiveLevelData : (Json.Decode.Value -> msg) -> Sub msg


port writeLevelData : String -> Cmd msg


port getControllerState : Int -> Cmd msg


port receiveControllerState : ({ up : Bool, left : Bool, right : Bool, down : Bool, jump : Bool, dash : Bool } -> msg) -> Sub msg


levelDataDecodeHandler : Json.Decode.Value -> Msg
levelDataDecodeHandler levelDataJson =
    case jsonToLevelData levelDataJson of
        Ok levelData ->
            ReceiveLevelData levelData

        Err errorMessage ->
            let
                _ =
                    Debug.log "Error in levelDataDecodeHandler" errorMessage
            in
                NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs (\dt -> GetControllerState)
        , receiveControllerState (\buttons -> Tick buttons)
        , Window.resizes (\size -> SetCanvasSize size)
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , receiveLevelData levelDataDecodeHandler
        , Mouse.moves (\{ x, y } -> MouseMove ( toFloat x, toFloat y ))
        , Mouse.clicks (\{ x, y } -> MouseClick ( toFloat x, toFloat y ))
        ]
