module Screens.NormalPlay exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Controller exposing (ControllerState)
import GameTypes exposing (Vector)
import Coordinates exposing (gameSize, gridToPixelConversion, centerToBottomLeftLocationConverter, gridSquareSize)
import Player exposing (Player, PlayerState(..), updatePlayer, renderPlayer)
import Enemy exposing (Enemy, renderEnemy, updateEnemies)
import Wall exposing (Wall, renderWall, wallDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Color


type alias NormalPlayState =
    { player : Player
    , enemies : List Enemy
    , walls : List Wall
    , camera : Camera
    , resources : Resources
    , mouse : Vector
    }



-- {
--   "platforms": [
--     {"location": {"x": 0, "y": 0}},
--     {"location": {"x": 1, "y": 0}},
--     {"location": {"x": 2, "y": 0}},
--     {"location": {"x": 3, "y": 0}},
--     {"location": {"x": 5, "y": 0}},
--     {"location": {"x": 7, "y": 0}}
--   ]
-- }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) Running ( 40, 40 ) 0
        , enemies = []
        , walls = []
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , mouse = ( 0, 0 )
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
        { player = Player startingPoint ( 0, 0 ) Running ( 40, 40 ) 0
        , enemies = []
        , walls = platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , mouse = ( 0, 0 )
        }


type alias LevelData =
    { platforms : List Wall
    }


enemyOne : Enemy
enemyOne =
    { location = ( 500, -150 )
    , timeExisted = 0
    , size = ( 40, 40 )
    }


enemyTwo : Enemy
enemyTwo =
    { location = ( 800, -150 )
    , timeExisted = 0
    , size = ( 40, 40 )
    }


enemyThree : Enemy
enemyThree =
    { location = ( 1100, -150 )
    , timeExisted = 0
    , size = ( 40, 40 )
    }


updateNormalPlay : ControllerState -> NormalPlayState -> NormalPlayState
updateNormalPlay controllerState state =
    let
        newEnemies =
            updateEnemies state.enemies

        newPlayer =
            updatePlayer state.enemies state.walls controllerState state.player
    in
        { state
            | player = newPlayer
            , enemies = newEnemies
            , camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
        }


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ (List.map renderEnemy state.enemies)
        , (List.map renderWall state.walls)
        , [ renderPlayer state.resources state.player ]
        , [ renderMouse state.mouse ]
        ]


renderMouse : Vector -> Renderable
renderMouse location =
    Render.rectangle
        { color = Color.charcoal
        , position = centerToBottomLeftLocationConverter location gridSquareSize
        , size = gridSquareSize
        }


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
        |> required "platforms" (Json.Decode.list wallDecoder)
