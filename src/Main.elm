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
import MouseEvents exposing (MouseEvent, onMouseMove, onClick, relPos, onMouseDown, onMouseUp)
import Keyboard.Extra
import Json.Decode
import GameTypes exposing (Vector, vectorIntToFloat)
import Coordinates exposing (gameSize, calculateCanvasSize)
import Controller
    exposing
        ( Controller
        , GamePad
        , calculateControllerStateFromKeyboardState
        , initialControllerState
        , calculateControllerStateFromGamePad
        , DPadHorizontal(DPadRight, DPadLeft, NoHorizontalDPad)
        , DPadVertical(DPadUp, DPadDown, NoVerticalDPad)
        , ButtonState
            ( Pressed
            , Held
            , Released
            , Inactive
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
        , TempProperties
        , initialTempProperties
        )
import CreateLevel
    exposing
        ( LevelCreateState
        , initialLevelCreateState
        , updatePlayStateAfterKeyPress
        , updatePlayStateAfterMouseClick
        , renderLevelCreateScreen
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
    , keyboard : Keyboard.Extra.State
    , controller : Controller
    , gameScreen : GameScreen
    , temporaryProperties : TempProperties
    }


type GameScreen
    = Uninitialized
    | CreateLevel LevelCreateState
    | NormalPlay NormalPlayState


type Msg
    = NoOp
    | SetWindowSize Window.Size
    | GetGamePadState
    | Tick GamePad
    | Resources Resources.Msg
    | KeyboardMsg Keyboard.Extra.Msg
    | ReceiveLevelData LevelData
    | MouseMove Vector
    | SetIsCursorActive Bool
    | TweekJumpDuration Float
    | TweekMaxJumpHeight Float
    | TweekMinJumpHeight Float
    | TweekWallFriction Float
    | TweekMaxWalkingSpeed Float
    | TweekMaxRunningSpeed Float
    | TweekDPadAcceleration Float


initialModel : Model
initialModel =
    { windowSize = ( 0, 0 )
    , keyboard = Keyboard.Extra.initialState
    , controller = initialControllerState
    , gameScreen = CreateLevel initialLevelCreateState
    , temporaryProperties = initialTempProperties
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetWindowSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/player-background-glow.png" ]
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
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | framesToApex = framesToApex }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekMaxJumpHeight maxJumpHeight ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxJumpHeight = maxJumpHeight }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekMinJumpHeight minJumpHeight ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | minJumpHeight = minJumpHeight }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekWallFriction wallFriction ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | wallFriction = wallFriction }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekMaxWalkingSpeed maxWalkingSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxWalkingSpeed = maxWalkingSpeed }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekMaxRunningSpeed maxRunningSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxRunningSpeed = maxRunningSpeed }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekDPadAcceleration dPadAcceleration ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | dPadAcceleration = dPadAcceleration }
            in
                { model
                    | temporaryProperties = newTempProps
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
            { model
                | keyboard = Keyboard.Extra.update keyMsg model.keyboard
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
                        newLevelCreateState =
                            { levelCreateState
                                | cursorLocation = mousePosition
                            }
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
                        }
                            ! []

        SetIsCursorActive isActive ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                NormalPlay state ->
                    model ! []

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | cursorActive = isActive
                            }
                    in
                        { model
                            | gameScreen = CreateLevel newLevelCreateState
                        }
                            ! []

        GetGamePadState ->
            model ! [ getGamePadState 0 ]

        Tick gamepadState ->
            let
                updatedController =
                    case gamepadState.gamepadConnected of
                        True ->
                            model.controller
                                |> calculateControllerStateFromGamePad gamepadState

                        False ->
                            model.controller
                                |> calculateControllerStateFromKeyboardState model.keyboard
            in
                case model.gameScreen of
                    Uninitialized ->
                        { model
                            | controller = updatedController
                        }
                            ! []

                    NormalPlay gameState ->
                        { model
                            | controller = updatedController
                            , gameScreen =
                                NormalPlay <|
                                    updateNormalPlay
                                        updatedController
                                        gameState
                                        model.temporaryProperties
                        }
                            ! []

                    CreateLevel levelCreateState ->
                        let
                            newLevelCreateState =
                                updatePlayStateAfterKeyPress model.keyboard levelCreateState

                            { playState } =
                                newLevelCreateState

                            playStateAfterPausedUpdate =
                                if updatedController.start == Pressed then
                                    { playState
                                        | paused = not playState.paused
                                    }
                                else
                                    playState

                            updatedLocationForCameraToFollow =
                                case playStateAfterPausedUpdate.paused of
                                    True ->
                                        let
                                            vx =
                                                case updatedController.dPadHorizontal of
                                                    DPadRight ->
                                                        20

                                                    DPadLeft ->
                                                        -20

                                                    NoHorizontalDPad ->
                                                        0

                                            vy =
                                                case updatedController.dPadVertical of
                                                    DPadUp ->
                                                        20

                                                    DPadDown ->
                                                        -20

                                                    NoVerticalDPad ->
                                                        0
                                        in
                                            V2.add newLevelCreateState.locationForCameraToFollow ( vx, vy )

                                    False ->
                                        ( playStateAfterPausedUpdate.player.x, playStateAfterPausedUpdate.player.y )

                            updatedPlayState =
                                case playStateAfterPausedUpdate.paused of
                                    True ->
                                        { playStateAfterPausedUpdate
                                            | camera = Camera.follow 0.5 0.17 (V2.sub updatedLocationForCameraToFollow ( -100, -100 )) playStateAfterPausedUpdate.camera
                                        }

                                    False ->
                                        updateNormalPlay updatedController playStateAfterPausedUpdate model.temporaryProperties

                            updatedLevelCreateState =
                                { newLevelCreateState
                                    | playState = updatedPlayState
                                    , locationForCameraToFollow = updatedLocationForCameraToFollow
                                }

                            ( finalLevelCreateState, encodedLevelData ) =
                                updatePlayStateAfterMouseClick model.windowSize updatedLevelCreateState.cursorLocation updatedLevelCreateState.cursorActive model.keyboard updatedLevelCreateState

                            cmd =
                                case encodedLevelData of
                                    Just encodedLevel ->
                                        writeLevelData encodedLevel

                                    Nothing ->
                                        Cmd.none
                        in
                            { model
                                | controller = updatedController
                                , gameScreen = CreateLevel finalLevelCreateState
                            }
                                ! [ cmd ]


mouseMoveEventToMsg : MouseEvent -> Msg
mouseMoveEventToMsg mouseEvent =
    let
        { x, y } =
            relPos mouseEvent
    in
        MouseMove ( toFloat x, toFloat y )


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
                    ( levelCreateState.playState.camera, renderLevelCreateScreen model.windowSize levelCreateState )

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
                    , onMouseDown (\_ -> SetIsCursorActive True)
                    , onMouseUp (\_ -> SetIsCursorActive False)
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
                        , Html.Attributes.value (toString model.temporaryProperties.framesToApex)
                        , onInput (\stringNumber -> TweekJumpDuration <| clamp 1 128 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Max Jumping Height"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "512"
                        , Html.Attributes.min (toString model.temporaryProperties.minJumpHeight)
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.maxJumpHeight)
                        , onInput (\stringNumber -> TweekMaxJumpHeight <| clamp model.temporaryProperties.minJumpHeight 512 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Min Jumping Height"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max (toString model.temporaryProperties.maxJumpHeight)
                        , Html.Attributes.min "16"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.minJumpHeight)
                        , onInput (\stringNumber -> TweekMinJumpHeight <| clamp 16 model.temporaryProperties.maxJumpHeight <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Wall Friction"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "0.9"
                        , Html.Attributes.min "0"
                        , Html.Attributes.step "0.05"
                        , Html.Attributes.value (toString model.temporaryProperties.wallFriction)
                        , onInput (\stringNumber -> TweekWallFriction <| clamp 0 0.9 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Max Walking Speed"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "25"
                        , Html.Attributes.min "5"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.maxWalkingSpeed)
                        , onInput (\stringNumber -> TweekMaxWalkingSpeed <| clamp 5 25 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Max Running Speed"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "50"
                        , Html.Attributes.min <| toString model.temporaryProperties.maxWalkingSpeed
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.maxRunningSpeed)
                        , onInput (\stringNumber -> TweekMaxRunningSpeed <| clamp model.temporaryProperties.maxWalkingSpeed 50 <| Result.withDefault 0 (String.toFloat stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "DPad Acceleration"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "3"
                        , Html.Attributes.min "0.1"
                        , Html.Attributes.step "0.1"
                        , Html.Attributes.value (toString model.temporaryProperties.dPadAcceleration)
                        , onInput (\stringNumber -> TweekDPadAcceleration <| clamp 0.1 3 <| Result.withDefault 0 (String.toFloat stringNumber))
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
                    [ h3 [ sideMargin ] [ text (toString 9) ]
                    , Html.p [] [ text "- Remove Platform" ]
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


port receiveGamePadState : (GamePad -> msg) -> Sub msg


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
