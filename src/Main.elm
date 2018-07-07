port module Main exposing (main)

-- system

import Task
import Html exposing (program, Html, div, h3, text, input)
import Html.Attributes exposing (style, max, min, step, value, type_)
import Html.Events exposing (onInput)
import Json.Decode


-- Libraries

import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Vector2 as V2 exposing (getX, getY)
import Game.TwoD.Camera as Camera
import AnimationFrame
import Window
import MouseEvents exposing (MouseEvent, onMouseMove, onClick, relPos, onMouseDown, onMouseUp)
import Keyboard.Extra


-- My Modules

import GameTypes exposing (Vector, TempProperties, vectorIntToFloat)
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
        , NormalPlayState
        , initialNormalPlayState
        , createLevel
        , updateNormalPlay
        , renderNormalPlay
        , jsonToLevelData
        )
import CreateLevel
    exposing
        ( LevelCreateState
        , initialLevelCreateState
        , renderLevelCreateScreen
        , updateCreateLevelState
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
    | TweekJumpDuration Int
    | TweekMaxJumpHeight Float
    | TweekMinJumpHeight Float
    | TweekMaxWallSlideSpeed Float
    | TweekMaxWalkingSpeed Float
    | TweekMaxRunningSpeed Float
    | TweekMaxDashingSpeed Float
    | TweekDPadAcceleration Float
    | TweekDashDuration Int
    | TweekDashRecoveryDuration Int
    | TweekButtonPressWindow Int


initialTempProperties : TempProperties
initialTempProperties =
    { framesToApex = 28
    , maxJumpHeight = 320
    , minJumpHeight = 16
    , maxWallSlideSpeed = 3
    , maxWalkingSpeed = 10
    , maxRunningSpeed = 20
    , maxDashingSpeed = 25
    , dPadAcceleration = 0.5
    , dashDuration = 41
    , dashRecoveryDuration = 41
    , buttonPressWindow = 13
    }


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
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/jumping-blob-sprite-sheet.png" ]
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/running-blob-sprite-sheet.png" ]
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/dashing-blob-sprite-sheet.png" ]
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/tripping-blob-sprite-sheet.png" ]
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/recovering-blob-sprite-sheet.png" ]
          , Cmd.map Resources <| Resources.loadTextures [ "./assets/idling-blob-sprite-sheet.png" ]
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

        TweekMaxWallSlideSpeed maxWallSlideSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxWallSlideSpeed = maxWallSlideSpeed }
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

        TweekMaxDashingSpeed maxDashingSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxDashingSpeed = maxDashingSpeed }
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

        TweekDashDuration dashDuration ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | dashDuration = dashDuration }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekDashRecoveryDuration dashRecoveryDuration ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | dashRecoveryDuration = dashRecoveryDuration }
            in
                { model
                    | temporaryProperties = newTempProps
                }
                    ! []

        TweekButtonPressWindow buttonPressWindow ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | buttonPressWindow = buttonPressWindow }
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
                newController =
                    updateController model.keyboard gamepadState model.controller

                ( newGameScreen, cmd ) =
                    updateGameScreen model.temporaryProperties model.keyboard model.windowSize model.gameScreen newController
            in
                { model
                    | gameScreen = newGameScreen
                    , controller = newController
                }
                    ! [ cmd ]


updateController : Keyboard.Extra.State -> GamePad -> Controller -> Controller
updateController keyboard gamePad controller =
    case gamePad.gamepadConnected of
        True ->
            calculateControllerStateFromGamePad gamePad controller

        False ->
            calculateControllerStateFromKeyboardState keyboard controller


updateGameScreen : TempProperties -> Keyboard.Extra.State -> Vector -> GameScreen -> Controller -> ( GameScreen, Cmd Msg )
updateGameScreen temporaryProperties keyboard windowSize gameScreen controller =
    case gameScreen of
        Uninitialized ->
            ( Uninitialized, Cmd.none )

        NormalPlay gameState ->
            ( NormalPlay <|
                updateNormalPlay
                    controller
                    temporaryProperties
                    gameState
            , Cmd.none
            )

        CreateLevel levelCreateState ->
            let
                ( newLevelCreateState, encodedLevelData ) =
                    updateCreateLevelState controller windowSize keyboard temporaryProperties levelCreateState

                cmd =
                    case encodedLevelData of
                        Just encodedLevel ->
                            writeLevelData encodedLevel

                        Nothing ->
                            Cmd.none
            in
                ( CreateLevel newLevelCreateState, cmd )


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
                        , onInput (\stringNumber -> TweekJumpDuration <| clamp 1 128 <| Result.withDefault 0 (String.toInt stringNumber))
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
                    [ text "Max Wall Slide Speed "
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "10"
                        , Html.Attributes.min "0"
                        , Html.Attributes.step "0.5"
                        , Html.Attributes.value (toString model.temporaryProperties.maxWallSlideSpeed)
                        , onInput (\stringNumber -> TweekMaxWallSlideSpeed <| clamp 0 10 <| Result.withDefault 0 (String.toFloat stringNumber))
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
                    [ text "Max Dashing Speed"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "50"
                        , Html.Attributes.min <| toString model.temporaryProperties.maxRunningSpeed
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.maxDashingSpeed)
                        , onInput (\stringNumber -> TweekMaxDashingSpeed <| clamp model.temporaryProperties.maxRunningSpeed 50 <| Result.withDefault 0 (String.toFloat stringNumber))
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
                , div []
                    [ text "Dash Duration in frames"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "50"
                        , Html.Attributes.min "25"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.dashDuration)
                        , onInput (\stringNumber -> TweekDashDuration <| clamp 25 50 <| Result.withDefault 0 (String.toInt stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Dash Recover Duration in frames"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "50"
                        , Html.Attributes.min "25"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.dashRecoveryDuration)
                        , onInput (\stringNumber -> TweekDashRecoveryDuration <| clamp 25 50 <| Result.withDefault 0 (String.toInt stringNumber))
                        ]
                        []
                    ]
                , div []
                    [ text "Dash button press window duration"
                    , input
                        [ type_ "number"
                        , Html.Attributes.max "18"
                        , Html.Attributes.min "8"
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (toString model.temporaryProperties.buttonPressWindow)
                        , onInput (\stringNumber -> TweekButtonPressWindow <| clamp 8 18 <| Result.withDefault 0 (String.toInt stringNumber))
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
