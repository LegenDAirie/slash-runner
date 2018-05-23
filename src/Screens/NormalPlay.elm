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

-- Libraries

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Color


-- My Modules

import Enemy exposing (Enemy)
import Coordinates exposing (gameSize)
import Controller exposing (Controller)
import GameTypes exposing (IntVector, Player, TempProperties)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder)
import Player
    exposing
        ( renderPlayer
        , initialPlayer
        , calculateAction
        , actionUpdate
        , updateRoutineX
        , collisionX
        , updateRoutineY
        , collisionY
        )


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : Dict IntVector Platform
    , camera : Camera
    , resources : Resources
    , paused : Bool
    }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = initialPlayer
        , permanentEnemies = []
        , enemies = []
        , platforms = Dict.empty
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , paused = False
        }


createLevel : LevelData -> NormalPlayState
createLevel levelData =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = initialPlayer
        , platforms = levelData.platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        , paused = False
        }


type alias LevelData =
    { platforms : Dict IntVector Platform }


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    let
        { player, platforms } =
            state

        newPlayer =
            calculateAction tempProperties controller platforms player
                |> actionUpdate tempProperties player
                |> updateRoutineX tempProperties controller
                |> collisionX platforms
                |> updateRoutineY tempProperties controller
                |> collisionY platforms
    in
        { state | player = newPlayer }


renderNormalPlay : NormalPlayState -> ( Int, Int ) -> List Renderable
renderNormalPlay state ( dash, recover ) =
    List.concat
        [ (List.map (\( gridCoordinate, platform ) -> renderPlatform Color.grey gridCoordinate) (Dict.toList state.platforms))
        , renderPlayer state.resources state.player ( dash, recover )
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
