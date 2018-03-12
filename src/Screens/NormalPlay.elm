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
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder, platformSize)
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
        , PersistantPlayerState(Dead, Dashing)
        , PlayerStateThisFrame
            ( SlidingOnWall
            , OnTheGround
            , JumpingFalling
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
        , DPadHorizontal(DPadRight, DPadLeft, NoHorizontalDPad)
        , DPadVertical(DPadUp, DPadDown, NoVerticalDPad)
        )
import CollisionHelpers
    exposing
        ( getOverlappingGridSquareCoord
        , getDisplacement
        , CollisionDirection(CollisionNegativeDirection, CollisionPositiveDirection)
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
        { player = Player -300 0 0 0 ( 128, 128 ) ( 64, 64 ) Nothing
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
        { player = Player -300 0 0 0 ( 128, 128 ) ( 64, 64 ) Nothing
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


capPlayerVelocity : Float -> Float
capPlayerVelocity velocity =
    clamp -50 50 velocity


calculateYGravityFromJumpProperties : Float -> Float -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    (2 * maxJumpHeight) / (framesToApex * framesToApex)


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs ((initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight)))


type Direction
    = Left
    | Right


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    -- leave this function nice and huge, no need to abstract out to updateplayer, updateenemey or anything
    -- ideally one collision function will take in a player and enemy and return new versions of each
    -- it's ok if Elm code gets long! yay!
    let
        ---- dont apply ground friction if left or right is pressed
        ---- calc dpad forces
        ------ update player x
        ---- add forces to xVel
        ---- apply horizontal jump if needed
        ---- add xVel to xLocation
        ---- check for collision
        ---- displace player out of collision
        ---- is player sliding on wall?
        ---- calc gravity
        ------ update player y
        ---- add forces to yVel
        ---- apply vertical jump if needed
        ---- add yVel to yLocation
        ---- check for collision
        ---- displace player out of collision
        ----- calc new player state if not handled by everything else
        { player, platforms } =
            state

        ------------------------------------------------------------------------
        ------------------------------------ Forces ----------------------------
        ------------------------------------------------------------------------
        baseGravity =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                |> negate

        noFriction =
            1

        fullStop =
            0

        ---- dont apply ground friction if left or right is pressed
        groundFriction =
            case controller.dPadHorizontal of
                DPadRight ->
                    noFriction

                DPadLeft ->
                    noFriction

                NoHorizontalDPad ->
                    tempProperties.groundFriction

        ---- calc dpad forces
        dPadForce =
            case controller.dPadHorizontal of
                DPadRight ->
                    0.3

                DPadLeft ->
                    -0.3

                NoHorizontalDPad ->
                    0

        baseJumpVelocityY =
            calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight baseGravity

        wallJumpVelocityX =
            baseJumpVelocityY / 2

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

        inStateAbleToJumpOffWall =
            True

        isPlayerJumpingOffWall =
            if inStateAbleToJumpOffWall && controller.jump == Pressed then
                case ( wallToTheLeft, wallToTheRight ) of
                    ( True, True ) ->
                        Nothing

                    ( False, False ) ->
                        Nothing

                    ( True, False ) ->
                        Just Left

                    ( False, True ) ->
                        Just Right
            else
                Nothing

        ------------------------------------------------------------------------
        --------------------------------- Update Player ------------------------
        ------------------------------------------------------------------------
        ----- Player X
        playerXVelFirstUpdate =
            case isPlayerJumpingOffWall of
                Just direction ->
                    case direction of
                        Left ->
                            wallJumpVelocityX

                        Right ->
                            -wallJumpVelocityX

                Nothing ->
                    player.vx + dPadForce

        playerXVelCapped =
            capPlayerVelocity playerXVelFirstUpdate

        playerXLocationFirstUpdate =
            player.x + playerXVelCapped

        --- check for collision
        overlappingGridSquareCoords =
            getOverlappingGridSquareCoord ( playerXLocationFirstUpdate, player.y ) player.hitBoxSize platforms
                |> Set.fromList
                |> Set.toList

        getHorizontalDisplacement : Float -> CollisionDirection
        getHorizontalDisplacement =
            getDisplacement (getX player.hitBoxSize) playerXLocationFirstUpdate (getX platformSize)

        horizontalDisplacements =
            overlappingGridSquareCoords
                |> List.filter (\coord -> Dict.member coord platforms)
                |> List.map (\( x, y ) -> getHorizontalDisplacement <| toFloat x)

        --- displace out of collision
        --- update velocity accordingly
        ( playerXAfterDisplacement, playerVXAfterDisplacement ) =
            case horizontalDisplacements of
                [] ->
                    ( playerXLocationFirstUpdate, playerXVelCapped )

                collision :: rest ->
                    case collision of
                        CollisionNegativeDirection overlap ->
                            ( playerXLocationFirstUpdate + overlap, fullStop )

                        CollisionPositiveDirection overlap ->
                            ( playerXLocationFirstUpdate - overlap, fullStop )

        ----- Player Y
        earlyJumpTerminationVelocity =
            calculateEarlyJumpTerminationVelocity baseJumpVelocityY baseGravity tempProperties.maxJumpHeight tempProperties.minJumpHeight

        isPlayerSlidingOnWall =
            case horizontalDisplacements of
                [] ->
                    False

                collision :: rest ->
                    case collision of
                        CollisionNegativeDirection overlap ->
                            if controller.dPadHorizontal == DPadLeft then
                                True
                            else
                                False

                        CollisionPositiveDirection overlap ->
                            if controller.dPadHorizontal == DPadRight then
                                True
                            else
                                False

        playerYVelAfterGravity =
            case isPlayerSlidingOnWall of
                True ->
                    (baseGravity + player.vy) * tempProperties.wallFriction

                False ->
                    baseGravity + player.vy

        playerYVelFirstUpdate =
            case controller.jump of
                Pressed ->
                    baseJumpVelocityY

                Held ->
                    playerYVelAfterGravity

                Released ->
                    if playerYVelAfterGravity > earlyJumpTerminationVelocity then
                        earlyJumpTerminationVelocity
                    else
                        playerYVelAfterGravity

                Inactive ->
                    playerYVelAfterGravity

        playerYVelCapped =
            capPlayerVelocity playerYVelFirstUpdate

        playerYLocationFirstUpdate =
            player.x + playerYVelCapped

        overlappingGridSquareCoordsAgain =
            getOverlappingGridSquareCoord ( playerXAfterDisplacement, playerYLocationFirstUpdate ) player.hitBoxSize platforms
                |> Set.fromList
                |> Set.toList

        getVerticalDisplacement : Float -> CollisionDirection
        getVerticalDisplacement =
            getDisplacement (getX player.hitBoxSize) playerYLocationFirstUpdate (getX platformSize)

        verticalDisplacements =
            overlappingGridSquareCoordsAgain
                |> List.filter (\coord -> Dict.member coord platforms)
                |> List.map (\( x, y ) -> getVerticalDisplacement <| toFloat y)

        ( playerYAfterDisplacement, playerVYAfterDisplacement ) =
            case verticalDisplacements of
                [] ->
                    ( playerYLocationFirstUpdate, playerYVelCapped )

                collision :: rest ->
                    case collision of
                        CollisionNegativeDirection overlap ->
                            ( playerYLocationFirstUpdate + overlap, noFriction )

                        CollisionPositiveDirection overlap ->
                            ( playerYLocationFirstUpdate - overlap, fullStop )

        updatedPlayer =
            { player
                | x = playerXAfterDisplacement
                , y = playerYAfterDisplacement
                , vx = playerVXAfterDisplacement
                , vy = playerVYAfterDisplacement
            }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub ( playerXAfterDisplacement, playerYAfterDisplacement ) ( -100, -100 )) state.camera
            , player = updatedPlayer
        }


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
