module Screens.NormalPlay
    exposing
        ( NormalPlayState
        , initialNormalPlayState
        , renderNormalPlay
        , LevelData
        , createLevel
        , updateNormalPlay
        , jsonToLevelData
        )

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2
import GameTypes exposing (Vector, Player)
import Coordinates exposing (gameSize, gridToPixelConversion)
import Player exposing (renderPlayer)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Controller
    exposing
        ( ControllerState
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


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : List Platform
    , camera : Camera
    , resources : Resources
    }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0
        , permanentEnemies = []
        , enemies = []
        , platforms = []
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        }


createLevel : LevelData -> NormalPlayState
createLevel levelData =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize

        platforms =
            List.map (\platform -> { platform | location = gridToPixelConversion platform.location }) levelData.platforms
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0
        , platforms = platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        }


type alias LevelData =
    { platforms : List Platform
    }


getAcceleration : DPad -> Vector
getAcceleration dPad =
    case dPad of
        Up ->
            ( 0, 0.1 )

        UpRight ->
            ( 0.1, 0.1 )

        Right ->
            ( 0.1, 0 )

        DownRight ->
            ( 0.1, -0.1 )

        Down ->
            ( 0, -0.1 )

        DownLeft ->
            ( -0.1, -0.1 )

        Left ->
            ( -0.1, 0 )

        UpLeft ->
            ( -0.1, 0.1 )

        NoDirection ->
            ( 0, 0 )


updateNormalPlay : ControllerState -> NormalPlayState -> NormalPlayState
updateNormalPlay controllerState state =
    let
        { player } =
            state

        acceleration =
            getAcceleration controllerState.dPad

        newVelocity =
            V2.add player.velocity acceleration

        newLocation =
            V2.add player.location newVelocity

        updatedPlayer =
            { player
                | location = newLocation
                , velocity = newVelocity
            }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , player = updatedPlayer
        }


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ (List.map (renderPlatform state.resources) state.platforms)
        , [ renderPlayer state.resources state.player ]
        ]


jsonToLevelData : Json.Decode.Value -> Result String LevelData
jsonToLevelData levelDataJson =
    Json.Decode.decodeValue levelDataDecoder levelDataJson


levelDataDecoder : Decoder LevelData
levelDataDecoder =
    decode LevelData
        |> required "platforms" (Json.Decode.list platformDecoder)
