module Screens.NormalPlay exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Controller exposing (ControllerState)
import GameTypes exposing (Vector, Player, PlayerState(..))
import Coordinates exposing (gameSize, gridToPixelConversion, centerToBottomLeftLocationConverter, gridSquareSize)
import Player exposing (updatePlayer, renderPlayer)
import Enemy exposing (Enemy, renderEnemy, updateEnemies, enemyDecoder, gridToPixelEnemyConvert)
import GamePlatform exposing (Platform, renderPlatform, platformDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


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
        { player = Player startingPoint ( 0, 0 ) Running ( 64, 64 ) 0
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

        enemies =
            List.map (\enemy -> gridToPixelEnemyConvert enemy) levelData.enemies
    in
        { player = Player startingPoint ( 0, 0 ) Running ( 64, 64 ) 0
        , permanentEnemies = enemies
        , enemies = enemies
        , platforms = platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        }


type alias LevelData =
    { platforms : List Platform
    , enemies : List Enemy
    }


updateNormalPlay : ControllerState -> NormalPlayState -> NormalPlayState
updateNormalPlay controllerState state =
    let
        newEnemies =
            updateEnemies state.player state.enemies

        newPlayer =
            updatePlayer state.enemies state.platforms controllerState state.player
    in
        { state
            | player = newPlayer
            , enemies = newEnemies
            , camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
        }


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ (List.concatMap renderEnemy state.enemies)
        , (List.map renderPlatform state.platforms)
        , [ renderPlayer state.resources state.player ]
        ]


renderBackground : Resources -> List Renderable
renderBackground resources =
    [ Render.spriteWithOptions
        { position = ( -1024, -1024, 0 )
        , size = ( 6144, 2048 )
        , texture = Resources.getTexture "../assets/background-square.jpg" resources
        , rotation = 0
        , pivot = ( 0, 0 )
        , tiling = ( 6, 2 )
        }
    ]


jsonToLevelData : Json.Decode.Value -> Result String LevelData
jsonToLevelData levelDataJson =
    Json.Decode.decodeValue levelDataDecoder levelDataJson


levelDataDecoder : Decoder LevelData
levelDataDecoder =
    decode LevelData
        |> required "platforms" (Json.Decode.list platformDecoder)
        |> required "enemies" (Json.Decode.list enemyDecoder)
