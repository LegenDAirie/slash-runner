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
import Mouse
import GamePlatform exposing (Platform, platformSize)
import CreateLevel exposing (LevelCreateState, initialLevelCreateState, updatePlayStateAfterKeyPress, updatePlayStateAfterMouseClick, renderLevelCreateScreen)
import MouseHelpers exposing (mouseToGridInPixels)


type alias Model =
    { canvasSize : Vector
    , keyboardState : Keyboard.Extra.State
    , controllerState : ControllerState
    , gameScreen : GameScreen
    }


type GameScreen
    = Uninitialized
    | CreateLevel LevelCreateState
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
    , gameScreen = CreateLevel initialLevelCreateState
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetCanvasSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/tile-bricks-test.png" ]
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
                            mouseToGridInPixels model.canvasSize playState.camera mousePosition

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
                        ( newLevelCreateState, encodedLevelData ) =
                            updatePlayStateAfterMouseClick model.canvasSize mousePosition model.keyboardState levelCreateState
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
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
