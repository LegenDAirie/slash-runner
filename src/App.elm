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
import Controller exposing (ButtonState(..), calculateButtonState, DPad(..), ControllerState, calculateControllerState, initialControllerState)
import Coordinates exposing (gameSize)
import Screens.NormalPlay exposing (initialNormalPlayState, LevelData, createLevel, updateNormalPlay, renderNormalPlay, NormalPlayState, jsonToLevelData)
import Keyboard.Extra
import Json.Decode exposing (Decoder)


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
    | Tick
    | Resources Resources.Msg
    | KeyboardMsg Keyboard.Extra.Msg
    | ReceiveLevelData LevelData


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
            { model
                | keyboardState = Keyboard.Extra.update keyMsg model.keyboardState
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
            let
                _ =
                    Debug.log "level data" levelData
            in
                { model
                    | gameScreen = NormalPlay (createLevel levelData)
                }
                    ! []

        Tick ->
            let
                newModel =
                    { model
                        | controllerState = calculateControllerState model.keyboardState model.controllerState
                    }
            in
                case newModel.gameScreen of
                    Uninitialized ->
                        newModel ! []

                    NormalPlay state ->
                        { newModel
                            | gameScreen =
                                NormalPlay <|
                                    updateNormalPlay
                                        model.controllerState
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
        [ AnimationFrame.diffs (\dt -> Tick)
        , Window.resizes (\size -> SetCanvasSize size)
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , receiveLevelData levelDataDecodeHandler
        ]
