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
import Vector2 as V2 exposing (getX, getY)
import GameTypes exposing (Vector, IntVector, Player, vectorFloatToInt)
import Player exposing (renderPlayer)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Coordinates exposing (gameSize, pixelToGridConversion, gridToPixelConversion)
import Color
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
import CollisionHelpers
    exposing
        ( getCollidingTiles
        , calculateLocationAndVelocityFromCollision
        , getCollisionDisplacementVector
        )


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : Dict IntVector Platform
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
        , platforms = Dict.empty
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
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0
        , platforms = levelData.platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        }


type alias LevelData =
    { platforms : Dict IntVector Platform
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
        { player, platforms } =
            state

        acceleration =
            getAcceleration controllerState.dPad

        velocityAfterAcceleration =
            V2.add player.velocity acceleration

        locationAfterMovement =
            V2.add player.location velocityAfterAcceleration

        collidingTileGridCoords =
            getCollidingTiles (vectorFloatToInt locationAfterMovement) velocityAfterAcceleration player.size platforms

        ( locationAfterCollision, velocityAfterCollision ) =
            calculateLocationAndVelocityFromCollision locationAfterMovement velocityAfterAcceleration player.size collidingTileGridCoords platforms

        updatedPlayer =
            { player
                | location = locationAfterCollision
                , velocity = velocityAfterCollision
            }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , player = updatedPlayer
        }


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    let
        collidingTiles =
            getCollidingTiles (vectorFloatToInt state.player.location) state.player.velocity state.player.size state.platforms
    in
        List.concat
            [ (List.map (\( gridCoordinate, platform ) -> renderPlatform Color.grey gridCoordinate) (Dict.toList state.platforms))
            , (List.map (renderPlatform Color.green) collidingTiles)
            , [ renderPlayer state.resources state.player ]
            ]


jsonToLevelData : Json.Decode.Value -> Result String LevelData
jsonToLevelData levelDataJson =
    Json.Decode.decodeValue levelDataDecoder levelDataJson


levelDataDecoder : Decoder LevelData
levelDataDecoder =
    let
        platforms =
            Json.Decode.list platformWithLocationsDecoder
                |> Json.Decode.map Dict.fromList
    in
        decode LevelData
            |> required "platforms" platforms
