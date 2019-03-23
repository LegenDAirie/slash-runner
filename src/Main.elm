port module Main exposing (main)

-- system
-- Libraries
-- My Modules

import Browser
import Browser.Dom
import Browser.Events
import Controller
    exposing
        ( Controller
        , GamePad
        , calculateControllerStateFromGamePad
        , calculateControllerStateFromKeyboardState
        , initialControllerState
        )
import Coordinates exposing (calculateCanvasSize, gameSize)
import CreateLevel
    exposing
        ( LevelCreateState
        , initialLevelCreateState
        , renderLevelCreateScreen
        , updateCreateLevelState
        )
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import GameTypes exposing (TempProperties, Vector, vectorIntToFloat)
import Html exposing (Html, div, h3, input, text)
import Html.Attributes exposing (max, min, step, style, type_, value)
import Html.Events exposing (onInput)
import Html.Events.Extra.Mouse
import Json.Decode
import Keyboard
import Screens.NormalPlay
    exposing
        ( LevelData
        , NormalPlayState
        , createLevel
        , initialNormalPlayState
        , jsonToLevelData
        , renderNormalPlay
        , updateNormalPlay
        )
import Task


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : ( Float, Float )
    , keyboard : List Keyboard.Key
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
    | GotNewWindowSize Int Int
    | GetGamePadState
    | Tick GamePad
    | KeyboardMsg Keyboard.Msg
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
    , buttonPressWindow = 13
    }


initialModel : Model
initialModel =
    { windowSize = ( 0, 0 )
    , keyboard = []
    , controller = initialControllerState
    , gameScreen = CreateLevel initialLevelCreateState
    , temporaryProperties = initialTempProperties
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Task.perform viewPortToWindowSize <| Browser.Dom.getViewport
        , fetchLevelData 1
        ]
    )


port fetchLevelData : Int -> Cmd msg


port writeLevelData : String -> Cmd msg


port getGamePadState : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TweekJumpDuration framesToApex ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | framesToApex = framesToApex }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMaxJumpHeight maxJumpHeight ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxJumpHeight = maxJumpHeight }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMinJumpHeight minJumpHeight ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | minJumpHeight = minJumpHeight }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMaxWallSlideSpeed maxWallSlideSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxWallSlideSpeed = maxWallSlideSpeed }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMaxWalkingSpeed maxWalkingSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxWalkingSpeed = maxWalkingSpeed }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMaxDashingSpeed maxDashingSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxDashingSpeed = maxDashingSpeed }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekMaxRunningSpeed maxRunningSpeed ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | maxRunningSpeed = maxRunningSpeed }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekDPadAcceleration dPadAcceleration ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | dPadAcceleration = dPadAcceleration }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekDashDuration dashDuration ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | dashDuration = dashDuration }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        TweekButtonPressWindow buttonPressWindow ->
            let
                { temporaryProperties } =
                    model

                newTempProps =
                    { temporaryProperties | buttonPressWindow = buttonPressWindow }
            in
            ( { model
                | temporaryProperties = newTempProps
              }
            , Cmd.none
            )

        GotNewWindowSize width height ->
            ( { model
                | windowSize = ( toFloat width, toFloat height )
              }
            , Cmd.none
            )

        KeyboardMsg keyMsg ->
            ( { model
                | keyboard = Keyboard.update keyMsg model.keyboard
              }
            , Cmd.none
            )

        ReceiveLevelData levelData ->
            case model.gameScreen of
                Uninitialized ->
                    ( model, Cmd.none )

                NormalPlay playState ->
                    ( { model
                        | gameScreen = NormalPlay (createLevel levelData)
                      }
                    , Cmd.none
                    )

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | playState = createLevel levelData
                            }
                    in
                    ( { model
                        | gameScreen = CreateLevel newLevelCreateState
                      }
                    , Cmd.none
                    )

        MouseMove mousePosition ->
            case model.gameScreen of
                Uninitialized ->
                    ( model, Cmd.none )

                NormalPlay state ->
                    ( model, Cmd.none )

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | cursorLocation = mousePosition
                            }
                    in
                    ( { model
                        | gameScreen = CreateLevel newLevelCreateState
                      }
                    , Cmd.none
                    )

        SetIsCursorActive isActive ->
            case model.gameScreen of
                Uninitialized ->
                    ( model, Cmd.none )

                NormalPlay state ->
                    ( model, Cmd.none )

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | cursorActive = isActive
                            }
                    in
                    ( { model
                        | gameScreen = CreateLevel newLevelCreateState
                      }
                    , Cmd.none
                    )

        GetGamePadState ->
            ( model, getGamePadState 0 )

        Tick gamepadState ->
            let
                newController =
                    updateController model.keyboard gamepadState model.controller

                ( newGameScreen, cmd ) =
                    updateGameScreen model.temporaryProperties model.keyboard model.windowSize model.gameScreen newController
            in
            ( { model
                | gameScreen = newGameScreen
                , controller = newController
              }
            , cmd
            )


updateController : List Keyboard.Key -> GamePad -> Controller -> Controller
updateController pressedKeys gamePad controller =
    case gamePad.gamepadConnected of
        True ->
            calculateControllerStateFromGamePad gamePad controller

        False ->
            calculateControllerStateFromKeyboardState pressedKeys controller


updateGameScreen : TempProperties -> List Keyboard.Key -> Vector -> GameScreen -> Controller -> ( GameScreen, Cmd Msg )
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


view : Model -> Browser.Document Msg
view model =
    { title = "Slash-Runner"
    , body = viewBody model
    }


viewBody : Model -> List (Html.Html Msg)
viewBody model =
    let
        ( camera, gameScene ) =
            case model.gameScreen of
                Uninitialized ->
                    ( Camera.fixedWidth (Tuple.first gameSize) ( 0, 0 ), [] )

                NormalPlay state ->
                    ( state.camera, renderNormalPlay state )

                CreateLevel levelCreateState ->
                    ( levelCreateState.playState.camera, renderLevelCreateScreen model.windowSize levelCreateState )

        canvasSize =
            calculateCanvasSize model.windowSize

        sideMargin =
            [ style "margin" "15px 10px" ]

        flexBoxRow =
            [ style "display" "flex", style "flex-direction" "row" ]
    in
    [ div
        [ style "display" "flex"
        , style "justify-content" "center"
        ]
        [ Game.renderWithOptions
            [ Html.Events.Extra.Mouse.onMove (MouseMove << .offsetPos)
            , Html.Events.Extra.Mouse.onDown (\_ -> SetIsCursorActive True)
            , Html.Events.Extra.Mouse.onUp (\_ -> SetIsCursorActive False)
            , style "border" "solid 1px black"
            ]
            { time = 0
            , size =
                ( floor <| Tuple.first canvasSize
                , floor <| Tuple.second canvasSize
                )
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
                , Html.Attributes.value (String.fromInt model.temporaryProperties.framesToApex)
                , onInput (\stringNumber -> TweekJumpDuration <| clamp 1 128 <| Maybe.withDefault 0 (String.toInt stringNumber))
                ]
                []
            ]
        , div []
            [ text "Max Jumping Height"
            , input
                [ type_ "number"
                , Html.Attributes.max "512"
                , Html.Attributes.min (String.fromFloat model.temporaryProperties.minJumpHeight)
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.maxJumpHeight)
                , onInput (\stringNumber -> TweekMaxJumpHeight <| clamp model.temporaryProperties.minJumpHeight 512 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , div []
            [ text "Min Jumping Height"
            , input
                [ type_ "number"
                , Html.Attributes.max (String.fromFloat model.temporaryProperties.maxJumpHeight)
                , Html.Attributes.min "16"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.minJumpHeight)
                , onInput (\stringNumber -> TweekMinJumpHeight <| clamp 16 model.temporaryProperties.maxJumpHeight <| Maybe.withDefault 0 (String.toFloat stringNumber))
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
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.maxWallSlideSpeed)
                , onInput (\stringNumber -> TweekMaxWallSlideSpeed <| clamp 0 10 <| Maybe.withDefault 0 (String.toFloat stringNumber))
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
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.maxWalkingSpeed)
                , onInput (\stringNumber -> TweekMaxWalkingSpeed <| clamp 5 25 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , div []
            [ text "Max Running Speed"
            , input
                [ type_ "number"
                , Html.Attributes.max "50"
                , Html.Attributes.min <| String.fromFloat model.temporaryProperties.maxWalkingSpeed
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.maxRunningSpeed)
                , onInput (\stringNumber -> TweekMaxRunningSpeed <| clamp model.temporaryProperties.maxWalkingSpeed 50 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , div []
            [ text "Max Dashing Speed"
            , input
                [ type_ "number"
                , Html.Attributes.max "50"
                , Html.Attributes.min <| String.fromFloat model.temporaryProperties.maxRunningSpeed
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.maxDashingSpeed)
                , onInput (\stringNumber -> TweekMaxDashingSpeed <| clamp model.temporaryProperties.maxRunningSpeed 50 <| Maybe.withDefault 0 (String.toFloat stringNumber))
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
                , Html.Attributes.value (String.fromFloat model.temporaryProperties.dPadAcceleration)
                , onInput (\stringNumber -> TweekDPadAcceleration <| clamp 0.1 3 <| Maybe.withDefault 0 (String.toFloat stringNumber))
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
                , Html.Attributes.value (String.fromInt model.temporaryProperties.dashDuration)
                , onInput (\stringNumber -> TweekDashDuration <| clamp 25 50 <| Maybe.withDefault 0 (String.toInt stringNumber))
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
                , Html.Attributes.value (String.fromInt model.temporaryProperties.buttonPressWindow)
                , onInput (\stringNumber -> TweekButtonPressWindow <| clamp 8 18 <| Maybe.withDefault 0 (String.toInt stringNumber))
                ]
                []
            ]
        ]
    , div
        [ style "display" "flex", style "flex-direction" "column" ]
        [ div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 0) ]
            , Html.p [] [ text "- PlaceNothing" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 1) ]
            , Html.p [] [ text "- Normal Platform" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 2) ]
            , Html.p [] [ text "- Static enemy that doesn't move" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 3) ]
            , Html.p [] [ text "- Dangerous platform" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 4) ]
            , Html.p [] [ text "- Enemy on a set path" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 5) ]
            , Html.p [] [ text "- Walking Enemy" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text (String.fromInt 9) ]
            , Html.p [] [ text "- Remove Platform" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text "R" ]
            , Html.p [] [ text "- Resets enemies to starting positions" ]
            ]
        , div
            flexBoxRow
            [ h3 sideMargin [ text "G + H" ]
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


viewPortToWindowSize : Browser.Dom.Viewport -> Msg
viewPortToWindowSize viewport =
    GotNewWindowSize (round viewport.viewport.width) (round viewport.viewport.height)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame (\_ -> GetGamePadState)
        , receiveGamePadState (\gamepadState -> Tick gamepadState)
        , Browser.Events.onResize GotNewWindowSize
        , Sub.map KeyboardMsg Keyboard.subscriptions
        , receiveLevelData levelDataDecodeHandler
        ]
