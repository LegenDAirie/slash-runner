port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Controller
import Coordinates
import CreateLevel
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import GameFeel
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse
import Json.Decode
import Keyboard
import NormalPlay
import Task
import V2


main : Program { width : Float, height : Float } Model Msg
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
    , controller : Controller.Controller
    , gameScreen : GameScreen
    , gameFeelProps : GameFeel.GameFeel
    }


type GameScreen
    = Uninitialized
    | CreateLevel CreateLevel.LevelCreateState
    | NormalPlay NormalPlay.NormalPlayState


type Msg
    = NoOp
    | GotNewWindowSize Int Int
    | GetGamePadState
    | Tick Controller.GamePad
    | KeyboardMsg Keyboard.Msg
    | ReceiveLevelData NormalPlay.LevelData
    | MouseMove V2.Vector2
    | SetIsCursorActive Bool
    | GotNewGameFeelAdjustment GameFeel.Msg


initialModel : { width : Float, height : Float } -> Model
initialModel { width, height } =
    { windowSize = ( width, height )
    , keyboard = []
    , controller = Controller.initialControllerState
    , gameScreen = CreateLevel CreateLevel.initialLevelCreateState
    , gameFeelProps = GameFeel.initialGameFeel
    }


init : { width : Float, height : Float } -> ( Model, Cmd Msg )
init screenSize =
    ( initialModel screenSize
    , Cmd.batch [ fetchLevelData 1 ]
    )


port fetchLevelData : Int -> Cmd msg


port writeLevelData : String -> Cmd msg


port getGamePadState : Int -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotNewGameFeelAdjustment gameFeelAdjustment ->
            ( { model
                | gameFeelProps = GameFeel.updateGameFeel gameFeelAdjustment model.gameFeelProps
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
                        | gameScreen = NormalPlay (NormalPlay.createLevel levelData)
                      }
                    , Cmd.none
                    )

                CreateLevel levelCreateState ->
                    let
                        newLevelCreateState =
                            { levelCreateState
                                | playState = NormalPlay.createLevel levelData
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
                    Controller.updateController model.keyboard gamepadState model.controller

                ( newGameScreen, cmd ) =
                    updateGameScreen model.gameFeelProps model.keyboard model.windowSize model.gameScreen newController
            in
            ( { model
                | gameScreen = newGameScreen
                , controller = newController
              }
            , cmd
            )


updateGameScreen : GameFeel.GameFeel -> List Keyboard.Key -> V2.Vector2 -> GameScreen -> Controller.Controller -> ( GameScreen, Cmd Msg )
updateGameScreen gameFeelProps keyboard windowSize gameScreen controller =
    case gameScreen of
        Uninitialized ->
            ( Uninitialized, Cmd.none )

        NormalPlay gameState ->
            ( NormalPlay <|
                NormalPlay.updateNormalPlay
                    controller
                    gameFeelProps
                    gameState
            , Cmd.none
            )

        CreateLevel levelCreateState ->
            let
                ( newLevelCreateState, encodedLevelData ) =
                    CreateLevel.updateCreateLevelState controller windowSize keyboard gameFeelProps levelCreateState

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
                    ( Camera.fixedWidth (Tuple.first Coordinates.gameSize) ( 0, 0 ), [] )

                NormalPlay state ->
                    ( state.camera, NormalPlay.renderNormalPlay state )

                CreateLevel levelCreateState ->
                    ( levelCreateState.camera, CreateLevel.renderLevelCreateScreen model.windowSize levelCreateState )

        canvasSize =
            Coordinates.calculateCanvasSize model.windowSize

        sideMargin =
            [ Html.Attributes.style "margin" "15px 10px" ]

        flexBoxRow =
            [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "row" ]
    in
    [ Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ Game.renderWithOptions
            [ Html.Events.Extra.Mouse.onMove (MouseMove << .offsetPos)
            , Html.Events.Extra.Mouse.onDown (\_ -> SetIsCursorActive True)
            , Html.Events.Extra.Mouse.onUp (\_ -> SetIsCursorActive False)
            , Html.Attributes.style "border" "solid 1px black"
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
    , gameFeelAdjustmentPanel model.gameFeelProps
    , Html.div
        [ Html.Attributes.style "display" "flex", Html.Attributes.style "flex-direction" "column" ]
        [ Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 0) ]
            , Html.p [] [ Html.text "- PlaceNothing" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 1) ]
            , Html.p [] [ Html.text "- Normal Platform" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 2) ]
            , Html.p [] [ Html.text "- Static enemy that doesn't move" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 3) ]
            , Html.p [] [ Html.text "- Dangerous platform" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 4) ]
            , Html.p [] [ Html.text "- Enemy on a set path" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 5) ]
            , Html.p [] [ Html.text "- Walking Enemy" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text (String.fromInt 9) ]
            , Html.p [] [ Html.text "- Remove Platform" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text "R" ]
            , Html.p [] [ Html.text "- Resets enemies to starting positions" ]
            ]
        , Html.div
            flexBoxRow
            [ Html.h3 sideMargin [ Html.text "G + H" ]
            , Html.p [] [ Html.text "- Wipe the whole level clean of everyting" ]
            ]
        ]
    ]


gameFeelAdjustmentPanel : GameFeel.GameFeel -> Html.Html Msg
gameFeelAdjustmentPanel =
    Html.map GotNewGameFeelAdjustment << GameFeel.gameFeelView


port receiveLevelData : (Json.Decode.Value -> msg) -> Sub msg


port receiveGamePadState : (Controller.GamePad -> msg) -> Sub msg


levelDataDecodeHandler : Json.Decode.Value -> Msg
levelDataDecodeHandler levelDataJson =
    case NormalPlay.jsonToLevelData levelDataJson of
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
