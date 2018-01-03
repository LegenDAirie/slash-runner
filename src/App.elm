port module App exposing (Model, Msg, view, init, update, subscriptions)

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
import Coordinates exposing (gameSize, convertToGameUnits, pixelToGridConversion, gridToPixelConversion, calculateCanvasSize)
import Screens.NormalPlay exposing (initialNormalPlayState, LevelData, createLevel, updateNormalPlay, renderNormalPlay, NormalPlayState, jsonToLevelData)
import Keyboard.Extra
import Json.Decode exposing (Decoder)
import Mouse
import GamePlatform exposing (Platform, platformSize)
import CreateLevel exposing (LevelCreateState, initialLevelCreateState, updatePlayStateAfterKeyPress, updatePlayStateAfterMouseClick, renderLevelCreateScreen)
import MouseHelpers exposing (mouseToGridInPixels)


type alias Model =
    { windowSize : Vector
    , keyboardState : Keyboard.Extra.State
    , controllerState : ControllerState
    , gameScreen : GameScreen
    }


type GameScreen
    = Uninitialized
    | CreateLevel LevelCreateState
    | NormalPlay NormalPlayState


type alias GamePadState =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    , jump : Bool
    , dash : Bool
    }


type Msg
    = NoOp
    | SetWindowSize Window.Size
    | GetGamePadState
    | Tick GamePadState
    | Resources Resources.Msg
    | KeyboardMsg Keyboard.Extra.Msg
    | ReceiveLevelData LevelData
    | MouseMove Vector
    | MouseClick Vector


initialModel : Model
initialModel =
    { windowSize = ( 0, 0 )
    , keyboardState = Keyboard.Extra.initialState
    , controllerState = initialControllerState
    , gameScreen = CreateLevel initialLevelCreateState
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetWindowSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/tile-bricks-test.png" ]
          , fetchLevelData 1
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetWindowSize size ->
            let
                windowSize =
                    (\windowSize -> ( toFloat windowSize.width, toFloat windowSize.height )) size
            in
                { model
                    | windowSize = windowSize
                }
                    ! []

        KeyboardMsg keyMsg ->
            let
                newKeyboardState =
                    Keyboard.Extra.update keyMsg model.keyboardState

                newModel =
                    { model
                        | keyboardState = newKeyboardState
                    }
            in
                case model.gameScreen of
                    Uninitialized ->
                        newModel
                            ! []

                    NormalPlay normalPlayState ->
                        newModel
                            ! []

                    CreateLevel levelCreateState ->
                        let
                            newPlayState =
                                updatePlayStateAfterKeyPress newKeyboardState levelCreateState
                        in
                            { newModel
                                | gameScreen = CreateLevel newPlayState
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

                CreateLevel levelCreateState ->
                    let
                        { itemToPlace, playState } =
                            levelCreateState

                        newPlayState =
                            { playState | resources = Resources.update msg playState.resources }

                        newLevelCreateState =
                            { levelCreateState
                                | playState = newPlayState
                            }
                    in
                        { model
                            | gameScreen =
                                CreateLevel newLevelCreateState
                        }
                            ! []

        ReceiveLevelData levelData ->
            -- let
            --     _ =
            --         Debug.log "level data" levelData
            -- in
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay playState ->
                    { model
                        | gameScreen = NormalPlay (createLevel levelData)
                    }
                        ! []

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | playState = createLevel levelData
                            }
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
                        }
                            ! []

        MouseMove mousePosition ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    model ! []

                CreateLevel levelCreateState ->
                    let
                        { itemToPlace, mouseLocation, playState } =
                            levelCreateState

                        ( width, height ) =
                            platformSize

                        newPosition =
                            mouseToGridInPixels model.windowSize playState.camera mousePosition

                        newLevelCreateState =
                            { levelCreateState
                                | mouseLocation = newPosition
                            }
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
                        }
                            ! []

        MouseClick mousePosition ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    model ! []

                CreateLevel levelCreateState ->
                    let
                        canvasSize =
                            calculateCanvasSize model.windowSize

                        ( newLevelCreateState, encodedLevelData ) =
                            updatePlayStateAfterMouseClick model.windowSize mousePosition model.keyboardState levelCreateState
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
                        }
                            ! [ writeLevelData encodedLevelData ]

        GetGamePadState ->
            let
                player1 =
                    1
            in
                model ! [ getGamePadState player1 ]

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

                    CreateLevel levelCreateState ->
                        let
                            { itemToPlace, playState } =
                                levelCreateState

                            newPlayState =
                                updateNormalPlay newControllerState playState

                            newLevelCreateState =
                                { levelCreateState
                                    | playState = newPlayState
                                }
                        in
                            { newModel
                                | gameScreen =
                                    CreateLevel newLevelCreateState
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

                CreateLevel levelCreateState ->
                    ( levelCreateState.playState.camera, renderLevelCreateScreen levelCreateState )

        canvasSize =
            calculateCanvasSize model.windowSize
    in
        div []
            [ Game.renderCenteredWithOptions
                []
                [ style [ ( "border", "solid 1px black" ) ] ]
                { time = 0
                , size = ( floor <| getX canvasSize, floor <| getY canvasSize )
                , camera = camera
                }
                gameScene
            ]


port fetchLevelData : Int -> Cmd msg


port receiveLevelData : (Json.Decode.Value -> msg) -> Sub msg


port writeLevelData : String -> Cmd msg


port getGamePadState : Int -> Cmd msg


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
        [ AnimationFrame.diffs (\dt -> GetGamePadState)
        , receiveControllerState (\buttons -> Tick buttons)
        , Window.resizes (\size -> SetWindowSize size)
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , receiveLevelData levelDataDecodeHandler
        , Mouse.moves (\{ x, y } -> MouseMove ( toFloat x, toFloat y ))
        , Mouse.clicks (\{ x, y } -> MouseClick ( toFloat x, toFloat y ))
        ]
