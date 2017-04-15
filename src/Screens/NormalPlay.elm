module Screens.NormalPlay exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Controller exposing (ControllerState)
import Coordinates exposing (gameSize, convertTouchCoorToGameCoor)
import Player exposing (Player, PlayerState(..), applyPhysics, renderPlayer, stateAfterPlatformCollision, stateAfterEnemyCollision, incrementPlayerCounters, stateAfterControllerInputs)
import Enemy exposing (Enemy, renderEnemy, updateEnemies)
import Wall exposing (Wall, renderWall)
import CollisionHelpers exposing (setByPlatform, getSideCollidingWithEnemies)


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


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( 0, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player =
            Player startingPoint ( 0, 0 ) Running ( 40, 40 ) 0
        , enemies = [ enemyOne, enemyTwo, enemyThree ]
        , walls = [ wallOne, wallTwo, wallThree ]
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        }


updateNormalPlay : ControllerState -> NormalPlayState -> NormalPlayState
updateNormalPlay controllerState state =
    let
        newEnemies =
            updateEnemies state.enemies

        ( setPlayerLocation, sideCollidingWithPlatform ) =
            setByPlatform state.player.location state.player.size state.walls Nothing

        sidecollidingWithEnemy =
            getSideCollidingWithEnemies state.player.location state.player.size state.enemies Nothing

        ( newPlayerState, newFramesSinceLastChain ) =
            ( state.player.playerState, state.player.framesSinceLastChain )
                |> incrementPlayerCounters
                |> stateAfterPlatformCollision sideCollidingWithPlatform
                |> stateAfterControllerInputs controllerState
                |> stateAfterEnemyCollision sidecollidingWithEnemy

        ( newLocation, newVelocity ) =
            applyPhysics controllerState.dPad newPlayerState newFramesSinceLastChain setPlayerLocation state.player.velocity

        thePlayer =
            state.player

        newPlayer =
            { thePlayer
                | location = newLocation
                , velocity = newVelocity
                , playerState = newPlayerState
                , framesSinceLastChain = newFramesSinceLastChain
            }
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
