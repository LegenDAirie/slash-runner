module Screens.NormalPlay exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Controller exposing (ControllerState)
import Coordinates exposing (gameSize)
import Player exposing (Player, PlayerState(..), updatePlayer, renderPlayer)
import Enemy exposing (Enemy, renderEnemy, updateEnemies)
import Wall exposing (Wall, renderWall)


type alias NormalPlayState =
    { player : Player
    , enemies : List Enemy
    , walls : List Wall
    , camera : Camera
    , resources : Resources
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


wallZero : Wall
wallZero =
    { location = ( 500, 100 )
    , size = ( 500, 100 )
    }


wallOne : Wall
wallOne =
    { location = ( -100, -200 )
    , size = ( 5000, 100 )
    }


wallTwo : Wall
wallTwo =
    { location = ( -600, 200 )
    , size = ( 100, 1000 )
    }


wallThree : Wall
wallThree =
    { location = ( -100, 200 )
    , size = ( 100, 1000 )
    }


wallFour : Wall
wallFour =
    { location = ( 500, -100 )
    , size = ( 50, 50 )
    }


wallFive : Wall
wallFive =
    { location = ( 591, -100 )
    , size = ( 50, 50 )
    }


wallSix : Wall
wallSix =
    { location = ( 682, -100 )
    , size = ( 50, 50 )
    }


wallSeven : Wall
wallSeven =
    { location = ( 773, -100 )
    , size = ( 50, 50 )
    }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player =
            Player startingPoint ( 0, 0 ) Running ( 40, 40 ) 0
        , enemies = []
        , walls = [ wallZero, wallOne, wallTwo, wallThree, wallFour, wallFive, wallSix, wallSeven ]
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
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
