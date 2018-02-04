port module Main exposing (main)

import Html exposing (program, Html, div, h3, text, input)
import Html.Attributes exposing (style, max, min, step, value, type_)
import Html.Events exposing (onInput)
import Vector2 as V2 exposing (getX, getY)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import AnimationFrame
import Window
import Task
import MouseEvents exposing (MouseEvent, onMouseMove, onClick, relPos)
import Keyboard.Extra
import Json.Decode
import GameTypes exposing (Vector, vectorIntToFloat)
import GamePlatform exposing (platformSize)
import MouseHelpers exposing (mouseToGridInPixels)
import Coordinates exposing (gameSize, calculateCanvasSize)
import Controller
    exposing
        ( ControllerState
        , GamePadState
        , calculateControllerStateFromKeyboardState
        , initialControllerState
        , calculateControllerStateFromGamePad
        , ButtonState
            ( Pressed
            , Held
            , Released
            , Inactive
            )
        , DPad
            ( Up
            , UpRight
            , Right
            , DownRight
            , Down
            , DownLeft
            , Left
            , UpLeft
            , NoDirection
            )
        )
import Screens.NormalPlay
    exposing
        ( LevelData
        , createLevel
        , updateNormalPlay
        , renderNormalPlay
        , NormalPlayState
        , jsonToLevelData
        , TempJumpProperties
        )
import CreateLevel
    exposing
        ( LevelCreateState
        , initialLevelCreateState
        , updatePlayStateAfterKeyPress
        , updatePlayStateAfterMouseClick
        , renderLevelCreateScreen
        , getLocationToFollowVelocity
        )


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Vector
    , keyboardState : Keyboard.Extra.State
    , controllerState : ControllerState
    , gameScreen : GameScreen
    , tempJumpProperties : TempJumpProperties
    }


type GameScreen
    = Uninitialized
    | CreateLevel LevelCreateState
    | NormalPlay NormalPlayState


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
    | TweekJumpDuration Float
    | TweekMaxJumpHeight Float
    | TweekMinJumpHeight Float


initialModel : Model
initialModel =
    { windowSize = ( 0, 0 )
    , keyboardState = Keyboard.Extra.initialState
    , controllerState = initialControllerState
    , gameScreen = CreateLevel initialLevelCreateState
    , tempJumpProperties = TempJumpProperties 28 256 16
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetWindowSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/tile-bricks-test.png" ]
          , fetchLevelData 1
          ]


port fetchLevelData : Int -> Cmd msg


port writeLevelData : String -> Cmd msg


port getGamePadState : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        TweekJumpDuration framesToApex ->
            { model
                | tempJumpProperties = TempJumpProperties framesToApex model.tempJumpProperties.maxJumpHeight model.tempJumpProperties.minJumpHeight
            }
                ! []

        TweekMaxJumpHeight maxJumpHeight ->
            { model
                | tempJumpProperties = TempJumpProperties model.tempJumpProperties.framesToApex maxJumpHeight model.tempJumpProperties.minJumpHeight
            }
                ! []

        TweekMinJumpHeight minJumpHeight ->
            { model
                | tempJumpProperties = TempJumpProperties model.tempJumpProperties.framesToApex model.tempJumpProperties.maxJumpHeight minJumpHeight
            }
                ! []

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
                                | mouseLocation = vectorIntToFloat newPosition
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
            model ! [ getGamePadState 0 ]

        Tick gamepadState ->
            let
                newControllerState =
                    case gamepadState.gamepadConnected of
                        True ->
                            model.controllerState
                                |> calculateControllerStateFromGamePad gamepadState

                        False ->
                            model.controllerState
                                |> calculateControllerStateFromKeyboardState model.keyboardState
            in
                case model.gameScreen of
                    Uninitialized ->
                        { model
                            | controllerState = newControllerState
                        }
                            ! []

                    NormalPlay state ->
                        { model
                            | controllerState = newControllerState
                            , gameScreen =
                                NormalPlay <|
                                    updateNormalPlay
                                        newControllerState
                                        state
                                        model.tempJumpProperties
                        }
                            ! []

                    CreateLevel levelCreateState ->
                        let
                            { playState } =
                                levelCreateState

                            playStateAfterPausedUpdate =
                                if newControllerState.start == Pressed then
                                    { playState
                                        | paused = not playState.paused
                                    }
                                else
                                    playState

                            updatedLocationForCameraToFollow =
                                case playStateAfterPausedUpdate.paused of
                                    True ->
                                        V2.add levelCreateState.locationForCameraToFollow (getLocationToFollowVelocity newControllerState.dPad)

                                    False ->
                                        playStateAfterPausedUpdate.player.location

                            updatedPlayState =
                                case playStateAfterPausedUpdate.paused of
                                    True ->
                                        { playStateAfterPausedUpdate
                                            | camera = Camera.follow 0.5 0.17 (V2.sub updatedLocationForCameraToFollow ( -100, -100 )) playStateAfterPausedUpdate.camera
                                        }

                                    False ->
                                        updateNormalPlay newControllerState playStateAfterPausedUpdate model.tempJumpProperties

                            newLevelCreateState =
                                { levelCreateState
                                    | playState = updatedPlayState
                                    , locationForCameraToFollow = updatedLocationForCameraToFollow
                                }
                        in
                            { model
                                | controllerState = newControllerState
                                , gameScreen = CreateLevel newLevelCreateState
                            }
                                ! []


mouseMoveEventToMsg : MouseEvent -> Msg
mouseMoveEventToMsg mouseEvent =
    let
        { x, y } =
            relPos mouseEvent
    in
        MouseMove ( toFloat x, toFloat y )


mouseClickEventToMsg : MouseEvent -> Msg
mouseClickEventToMsg mouseEvent =
    let
        { x, y } =
            relPos mouseEvent
    in
        MouseClick ( toFloat x, toFloat y )


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

        sideMargin =
            style [ ( "margin", "15px 10px" ) ]

        flexBoxRow =
            style [ ( "display", "flex" ), ( "flex-direction", "row" ) ]
    in
        div []
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ Game.renderWithOptions
                    [ onMouseMove mouseMoveEventToMsg
                    , onClick mouseClickEventToMsg
                    , style [ ( "border", "solid 1px black" ) ]
                    ]
                    { time = 0
                    , size = ( floor <| getX canvasSize, floor <| getY canvasSize )
                    , camera = camera
                    }
                    gameScene
                ]
            , div []
                [ div []
                    [ text "Frames to Apex"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "128"
                        , Html.Attributes.min "1"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.tempJumpProperties.framesToApex)
                        , onInput (\stringNumber -> TweekJumpDuration <| clamp 1 128 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Max Jumping Height"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "512"
                        , Html.Attributes.min (toString model.tempJumpProperties.minJumpHeight)
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.tempJumpProperties.maxJumpHeight)
                        , onInput (\stringNumber -> TweekMaxJumpHeight <| clamp model.tempJumpProperties.minJumpHeight 512 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Min Jumping Height"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max (toString model.tempJumpProperties.maxJumpHeight)
                        , Html.Attributes.min "16"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.tempJumpProperties.minJumpHeight)
                        , onInput (\stringNumber -> TweekMinJumpHeight <| clamp 16 model.tempJumpProperties.maxJumpHeight <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                ]
            , div
                [ style [ ( "display", "flex" ), ( "flex-direction", "column" ) ]
                ]
                [ div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 0) ]
                    , Html.p [] [ text "- PlaceNothing" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 1) ]
                    , Html.p [] [ text "- Normal Platform" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 2) ]
                    , Html.p [] [ text "- Static enemy that doesn't move" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 3) ]
                    , Html.p [] [ text "- Dangerous platform" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 4) ]
                    , Html.p [] [ text "- Enemy on a set path" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text (toString 5) ]
                    , Html.p [] [ text "- Walking Enemy" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text "R" ]
                    , Html.p [] [ text "- Resets enemies to starting positions" ]
                    ]
                , div
                    [ flexBoxRow ]
                    [ h3 [ sideMargin ] [ text "G + H" ]
                    , Html.p [] [ text "- Wipe the whole level clean of everyting" ]
                    ]
                ]
            ]


port receiveLevelData : (Json.Decode.Value -> msg) -> Sub msg


port receiveGamePadState : (GamePadState -> msg) -> Sub msg


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
        , receiveGamePadState (\gamepadState -> Tick gamepadState)
        , Window.resizes (\size -> SetWindowSize size)
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , receiveLevelData levelDataDecodeHandler
        ]
