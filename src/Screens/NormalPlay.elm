module Screens.NormalPlay
    exposing
        ( NormalPlayState
        , initialNormalPlayState
        , renderNormalPlay
        , LevelData
        , createLevel
        , updateNormalPlay
        , jsonToLevelData
        , TempProperties
        )

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Player exposing (renderPlayer)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Coordinates exposing (gameSize, pixelToGridConversion, gridToPixelConversion)
import Color
import Set
import GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorFloatToInt
        , PlayerState
            ( OnTheGround
            , Jumping
            , SlidingOnWall
            )
        )
import Controller
    exposing
        ( Controller
        , ButtonState
            ( Pressed
            , Held
            , Released
            , Inactive
            )
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
        , calculatePlayerAttributesFromCollision
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


type alias TempProperties =
    { framesToApex : Float
    , maxJumpHeight : Float
    , minJumpHeight : Float
    , groundFriction : Float
    , wallFriction : Float
    }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0 OnTheGround
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
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0 OnTheGround
        , platforms = levelData.platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        , paused = False
        }


type alias LevelData =
    { platforms : Dict IntVector Platform
    }


getDPadAcceleration : DPad -> Vector
getDPadAcceleration dPad =
    case dPad of
        Up ->
            ( 0, 0 )

        UpRight ->
            ( 0.3, 0 )

        Right ->
            ( 0.3, 0 )

        DownRight ->
            ( 0.3, 0 )

        Down ->
            ( 0, 0 )

        DownLeft ->
            ( -0.3, 0 )

        Left ->
            ( -0.3, 0 )

        UpLeft ->
            ( -0.3, 0 )

        NoDirection ->
            ( 0, 0 )


capPlayerVelocity : Vector -> Vector
capPlayerVelocity ( x, y ) =
    ( clamp -50 50 x, max y -50 )


calculateYGravityFromJumpProperties : Float -> Float -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    (2 * maxJumpHeight) / (framesToApex * framesToApex)


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs ((initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight)))


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    -- leave this function nice and huge, no need to abstract out to updateplayer, updateenemey or anything
    -- ideally one collision function will take in a player and enemy and return new versions of each
    -- it's ok if Elm code gets long! yay!
    let
        { player, platforms } =
            state

        gravitationalAcceleration =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                |> (\y -> ( 0, -y ))

        movementAcceleration =
            getDPadAcceleration controller.dPad

        finalPlayerAcceleration =
            List.foldr V2.add
                ( 0, 0 )
                [ gravitationalAcceleration
                , movementAcceleration
                ]

        playerVelocityAfterAcceleration =
            V2.add player.velocity finalPlayerAcceleration

        jumpVelocity =
            calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight (getY gravitationalAcceleration)
                |> (\y -> ( getX playerVelocityAfterAcceleration, y ))

        ( playerVelocityAfterJump, playerStateAfterJump ) =
            case controller.jump of
                Pressed ->
                    case player.playerState of
                        OnTheGround ->
                            ( jumpVelocity, Jumping )

                        SlidingOnWall ->
                            ( jumpVelocity, Jumping )

                        Jumping ->
                            ( playerVelocityAfterAcceleration, Jumping )

                Held ->
                    ( playerVelocityAfterAcceleration, player.playerState )

                Released ->
                    let
                        earlyJumpTerminationVelocity =
                            calculateEarlyJumpTerminationVelocity (getY jumpVelocity) (getY gravitationalAcceleration) tempProperties.maxJumpHeight tempProperties.minJumpHeight

                        newVelocity =
                            if (getY playerVelocityAfterAcceleration > earlyJumpTerminationVelocity) then
                                ( getX playerVelocityAfterAcceleration, earlyJumpTerminationVelocity )
                            else
                                playerVelocityAfterAcceleration
                    in
                        ( newVelocity, player.playerState )

                Inactive ->
                    ( playerVelocityAfterAcceleration, player.playerState )

        playerVelocityAfterCap =
            playerVelocityAfterJump
                |> capPlayerVelocity

        playerLocationAfterMovement =
            V2.add player.location playerVelocityAfterCap

        collidingTileGridCoords =
            getCollidingTiles playerLocationAfterMovement player.size platforms
                |> Set.fromList
                |> Set.toList

        groundFriction =
            calculateGroundFrictionFromControllerState tempProperties.groundFriction controller.dPad

        ( playerLocationAfterCollision, playerVelocityAfterCollision, playerStateAfterCollision ) =
            calculatePlayerAttributesFromCollision groundFriction tempProperties.wallFriction playerLocationAfterMovement playerVelocityAfterCap playerStateAfterJump player.size collidingTileGridCoords platforms

        updatedPlayer =
            { player
                | location = playerLocationAfterCollision
                , velocity = playerVelocityAfterCollision
                , playerState = playerStateAfterCollision
            }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , player = updatedPlayer
        }


calculateGroundFrictionFromControllerState : Float -> DPad -> Float
calculateGroundFrictionFromControllerState groundFriction dPad =
    let
        noFriction =
            1
    in
        case dPad of
            Up ->
                groundFriction

            Down ->
                groundFriction

            NoDirection ->
                groundFriction

            UpRight ->
                noFriction

            Right ->
                noFriction

            DownRight ->
                noFriction

            DownLeft ->
                noFriction

            Left ->
                noFriction

            UpLeft ->
                noFriction


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ (List.map (\( gridCoordinate, platform ) -> renderPlatform Color.grey gridCoordinate) (Dict.toList state.platforms))
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
