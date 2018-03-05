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
import Player exposing (renderPlayer, getPlayerLeftKickPoint, getPlayerRightKickPoint)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Coordinates exposing (gameSize, pixelToGridConversion, gridToPixelConversion, locationToGridCoordinate)
import Color
import Set
import GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorFloatToInt
        , vectorIntToFloat
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
        { player = Player startingPoint ( 0, 0 ) ( 128, 128 ) ( 64, 64 ) 0 OnTheGround
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
        { player = Player startingPoint ( 0, 0 ) ( 128, 128 ) ( 64, 64 ) 0 OnTheGround
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


getGravityAfterWallFriction : PlayerState -> Float -> Vector -> Vector
getGravityAfterWallFriction playerState wallFriction gravity =
    case playerState of
        OnTheGround ->
            gravity

        Jumping ->
            gravity

        SlidingOnWall ->
            ( getX gravity, wallFriction * getY gravity )


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    -- leave this function nice and huge, no need to abstract out to updateplayer, updateenemey or anything
    -- ideally one collision function will take in a player and enemy and return new versions of each
    -- it's ok if Elm code gets long! yay!
    let
        { player, platforms } =
            state

        baseGravity =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                |> (\y -> ( 0, -y ))

        gravitationalAcceleration =
            baseGravity
                |> getGravityAfterWallFriction player.playerState tempProperties.wallFriction

        playerVelocityAfterSlidingOnWall =
            if player.playerState == SlidingOnWall then
                ( getX player.velocity, tempProperties.wallFriction * getY player.velocity )
            else
                player.velocity

        movementAcceleration =
            getDPadAcceleration controller.dPad

        finalPlayerAcceleration =
            List.foldr V2.add
                ( 0, 0 )
                [ gravitationalAcceleration
                , movementAcceleration
                ]

        playerVelocityAfterAcceleration =
            V2.add playerVelocityAfterSlidingOnWall finalPlayerAcceleration

        jumpVelocity =
            calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight (getY baseGravity)
                |> (\y -> ( getX playerVelocityAfterAcceleration, y ))

        wallToTheRight =
            player
                |> getPlayerRightKickPoint
                |> locationToGridCoordinate
                |> flip Dict.member platforms

        wallToTheLeft =
            player
                |> getPlayerLeftKickPoint
                |> locationToGridCoordinate
                |> flip Dict.member platforms

        nearWallJumpVelocity =
            case ( wallToTheLeft, wallToTheRight ) of
                ( True, True ) ->
                    playerVelocityAfterAcceleration

                ( False, False ) ->
                    playerVelocityAfterAcceleration

                ( True, False ) ->
                    ( (getY jumpVelocity) / 2, getY jumpVelocity )

                ( False, True ) ->
                    ( -(getY jumpVelocity) / 2, getY jumpVelocity )

        ( playerVelocityAfterJump, playerStateAfterJump ) =
            case controller.jump of
                Pressed ->
                    case player.playerState of
                        OnTheGround ->
                            ( jumpVelocity, Jumping )

                        SlidingOnWall ->
                            ( nearWallJumpVelocity, Jumping )

                        Jumping ->
                            let
                                inFreeFall =
                                    getY playerVelocityAfterAcceleration <= -15

                                jumpYVelocity =
                                    jumpUpOrAway controller.dPad inFreeFall (getY playerVelocityAfterAcceleration) (getY nearWallJumpVelocity)
                            in
                                ( ( getX nearWallJumpVelocity, jumpYVelocity ), Jumping )

                Held ->
                    ( playerVelocityAfterAcceleration, player.playerState )

                Released ->
                    let
                        earlyJumpTerminationVelocity =
                            calculateEarlyJumpTerminationVelocity (getY jumpVelocity) (getY baseGravity) tempProperties.maxJumpHeight tempProperties.minJumpHeight

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
            getCollidingTiles playerLocationAfterMovement player.hitBoxSize platforms
                |> Set.fromList
                |> Set.toList

        groundFriction =
            calculateGroundFrictionFromControllerState tempProperties.groundFriction controller.dPad

        ( playerLocationAfterCollision, playerVelocityAfterCollision, playerStateAfterCollision ) =
            case List.any (\coordinate -> Dict.member coordinate platforms) collidingTileGridCoords of
                True ->
                    calculatePlayerAttributesFromCollision groundFriction tempProperties.wallFriction playerLocationAfterMovement playerVelocityAfterCap playerStateAfterJump player.hitBoxSize collidingTileGridCoords platforms

                False ->
                    ( playerLocationAfterMovement, playerVelocityAfterCap, Jumping )

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


jumpUpOrAway : DPad -> Bool -> Float -> Float -> Float
jumpUpOrAway dPad inFreeFall currentVelY jumpVelY =
    case inFreeFall of
        True ->
            case dPad of
                Up ->
                    jumpVelY

                UpRight ->
                    jumpVelY

                Right ->
                    currentVelY

                DownRight ->
                    currentVelY

                Down ->
                    currentVelY

                DownLeft ->
                    currentVelY

                Left ->
                    currentVelY

                UpLeft ->
                    jumpVelY

                NoDirection ->
                    currentVelY

        False ->
            jumpVelY


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
        , renderPlayer state.resources state.player
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
