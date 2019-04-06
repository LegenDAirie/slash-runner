module NormalPlay exposing
    ( LevelData
    , NormalPlayState
    , createLevel
    , initialNormalPlayState
    , jsonToLevelData
    , renderNormalPlay
    , resetPlayState
    , updateNormalPlay
    )

-- Libraries
-- My Modules

import Color
import Controller exposing (ButtonState, Controller)
import Coordinates exposing (gameSize)
import Dict exposing (Dict)
import Enemy exposing (Enemy)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import GamePlatform exposing (Platform, platformWithLocationsDecoder, renderPlatform)
import GameTypes exposing (Player, TempProperties)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Player
    exposing
        ( initialPlayer
        , renderPlayer
        , updatePlayer
        )
import V2


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : Dict V2.IntVector Platform
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



-- Helpers


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


resetPlayState : NormalPlayState -> NormalPlayState
resetPlayState normalPlayState =
    { normalPlayState
        | player = initialPlayer
        , camera = Camera.fixedWidth (Tuple.first gameSize) ( initialPlayer.x, initialPlayer.y )
        , enemies = normalPlayState.permanentEnemies
        , paused = True
    }


type alias LevelData =
    { platforms : Dict V2.IntVector Platform }


updatePausedState : ButtonState -> NormalPlayState -> NormalPlayState
updatePausedState startButton state =
    if startButton == Controller.Pressed then
        { state | paused = not state.paused }

    else
        state


updateNormalPlay : Controller -> TempProperties -> NormalPlayState -> NormalPlayState
updateNormalPlay controller tempProperties state =
    updatePausedState controller.startButton state
        |> updatePlayState controller tempProperties


updatePlayState : Controller -> TempProperties -> NormalPlayState -> NormalPlayState
updatePlayState controller tempProperties theState =
    if theState.paused then
        theState

    else
        { theState
            | player = updatePlayer controller tempProperties theState.platforms theState.player
        }
            |> (\state -> { state | camera = Camera.follow 0.5 0.17 ( state.player.x, state.player.y ) state.camera })


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ List.map (\( gridCoordinate, platform ) -> renderPlatform Color.grey gridCoordinate) (Dict.toList state.platforms)
        , renderPlayer state.resources state.player state.platforms
        ]


jsonToLevelData : Json.Decode.Value -> Result Json.Decode.Error LevelData
jsonToLevelData levelDataJson =
    Json.Decode.decodeValue levelDataDecoder levelDataJson


levelDataDecoder : Decoder LevelData
levelDataDecoder =
    let
        platforms =
            Json.Decode.list platformWithLocationsDecoder
                |> Json.Decode.map Dict.fromList
    in
    Json.Decode.succeed LevelData
        |> required "platforms" platforms
