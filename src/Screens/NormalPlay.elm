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
        , initialTempProperties
        )

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Player exposing (renderPlayer, getPlayerLeftKickPoint, getPlayerRightKickPoint, playerHitBoxSize)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder, platformSize)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Coordinates exposing (gameSize, pixelToGridConversion, gridToPixelConversion, locationToGridCoordinate)
import Color
import GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorFloatToInt
        , vectorIntToFloat
        , PersistantPlayerState(Dead, Dashing, OnTheGround)
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
        ( getOverlappingGridSquareCoords
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



------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------


type alias TempProperties =
    { framesToApex : Float
    , maxJumpHeight : Float
    , minJumpHeight : Float
    , wallFriction : Float
    , maxWalkingSpeed : Float
    , maxRunningSpeed : Float
    , dPadAcceleration : Float
    }


initialTempProperties : TempProperties
initialTempProperties =
    { framesToApex = 28
    , maxJumpHeight = 256
    , minJumpHeight = 16
    , wallFriction = 0
    , maxWalkingSpeed = 10
    , maxRunningSpeed = 30
    , dPadAcceleration = 0.5
    }



------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player 0 0 0 0 (Just OnTheGround)
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
        { player = Player 0 0 0 0 (Just OnTheGround)
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



--------------------------------------------------------------------------------
------------------------temporary functions ------------------------------------
--------------------------------------------------------------------------------
-- These are here for play testing and deciding what feels best. Once the game feel has
-- been decided these will be removed and replaced by constants


calculateYGravityFromJumpProperties : Float -> Float -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    (2 * maxJumpHeight) / (framesToApex * framesToApex)


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs ((initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight)))


calculateFriction : Float -> Float -> Float
calculateFriction acceleration maxSpeed =
    -((acceleration - maxSpeed) / maxSpeed)



------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
---------------------------Helper functions-------------------------------------
--------------------------------------------------------------------------------


type Direction
    = Left
    | Right


capPlayerVelocity : Float -> Float -> Float
capPlayerVelocity topSpeed velocity =
    clamp -topSpeed topSpeed velocity



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    -- leave this function nice and huge, no need to abstract out to updateplayer, updateenemey or anything
    -- it's ok if Elm code gets long! yay!
    let
        { player, platforms } =
            state

        noFriction =
            1

        fullStop =
            0

        maxFallSpeed =
            15

        ------------------------------------------------------------------------
        ------------------------------------ Forces ----------------------------
        ------------------------------------------------------------------------
        baseGravity =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                |> negate

        baseJumpVelocityY =
            calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight baseGravity

        wallJumpVelocityX =
            baseJumpVelocityY

        frictionWhileRunning =
            calculateFriction tempProperties.dPadAcceleration tempProperties.maxRunningSpeed

        frictionWhileWalking =
            calculateFriction tempProperties.dPadAcceleration tempProperties.maxWalkingSpeed

        horizontalFriction =
            if controller.dPadHorizontal == NoHorizontalDPad then
                -- Player is coming to a stop
                0.93
            else if controller.dash == Held then
                -- Player is running
                frictionWhileRunning
            else
                -- Player is walking
                frictionWhileWalking

        -- ---- calc dpad forces
        dPadForce =
            case controller.dPadHorizontal of
                DPadRight ->
                    tempProperties.dPadAcceleration

                DPadLeft ->
                    -tempProperties.dPadAcceleration

                NoHorizontalDPad ->
                    0

        getDirection number =
            if number < 0 then
                -1
            else
                1

        dashingVelocity =
            case controller.dPadHorizontal of
                DPadLeft ->
                    -tempProperties.maxRunningSpeed

                DPadRight ->
                    tempProperties.maxRunningSpeed

                NoHorizontalDPad ->
                    getDirection player.vx * tempProperties.maxWalkingSpeed

        earlyJumpTerminationVelocity =
            calculateEarlyJumpTerminationVelocity baseJumpVelocityY baseGravity tempProperties.maxJumpHeight tempProperties.minJumpHeight

        -- update things that happen no matter what state the player is in
        playerXVelAfterFriction =
            player.vx * horizontalFriction

        -- update the things are that specific to player states
        updatedPlayer =
            case player.playerState of
                Nothing ->
                    --- calculate what state the player is in
                    let
                        wallToTheRight =
                            ( player.x, player.y )
                                |> getPlayerRightKickPoint
                                |> locationToGridCoordinate
                                |> flip Dict.member platforms

                        wallToTheLeft =
                            ( player.x, player.y )
                                |> getPlayerLeftKickPoint
                                |> locationToGridCoordinate
                                |> flip Dict.member platforms

                        approximateWall =
                            case ( wallToTheLeft, wallToTheRight ) of
                                ( True, True ) ->
                                    Nothing

                                ( False, False ) ->
                                    Nothing

                                ( True, False ) ->
                                    Just Left

                                ( False, True ) ->
                                    Just Right

                        playerXvelUpdated =
                            case controller.jump of
                                Pressed ->
                                    case approximateWall of
                                        Just direction ->
                                            case direction of
                                                Left ->
                                                    wallJumpVelocityX

                                                Right ->
                                                    -wallJumpVelocityX

                                        Nothing ->
                                            playerXVelAfterFriction + dPadForce

                                _ ->
                                    playerXVelAfterFriction + dPadForce

                        playerXAfterLocationUpdate =
                            playerXvelUpdated + player.x

                        ----- how much the player is overlapping with platforms horizontally
                        horizontalDisplacement =
                            getOverlappingGridSquareCoords ( playerXAfterLocationUpdate, player.y ) playerHitBoxSize platforms
                                |> List.filter (\coord -> Dict.member coord platforms)
                                |> List.map (\( x, _ ) -> getDisplacement (getX playerHitBoxSize) playerXAfterLocationUpdate (getX platformSize) (toFloat x))
                                |> List.head

                        ----- move out of collision
                        ( playerXAfterDisplacement, playerVXAfterDisplacement, horizontalCollisionHappened ) =
                            case horizontalDisplacement of
                                Nothing ->
                                    ( playerXAfterLocationUpdate, playerXvelUpdated, False )

                                Just collision ->
                                    case collision of
                                        CollisionNegativeDirection overlap ->
                                            ( playerXAfterLocationUpdate + overlap, fullStop, True )

                                        CollisionPositiveDirection overlap ->
                                            ( playerXAfterLocationUpdate - overlap, fullStop, True )

                        playerYVelAfterGravity =
                            case controller.jump of
                                Pressed ->
                                    case approximateWall of
                                        Just _ ->
                                            baseJumpVelocityY

                                        Nothing ->
                                            baseGravity + player.vy

                                Released ->
                                    case horizontalDisplacement of
                                        Nothing ->
                                            min (baseGravity + player.vy) earlyJumpTerminationVelocity

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection _ ->
                                                    if controller.dPadHorizontal == DPadLeft then
                                                        (baseGravity + player.vy) * tempProperties.wallFriction
                                                    else
                                                        min (baseGravity + player.vy) earlyJumpTerminationVelocity

                                                CollisionPositiveDirection _ ->
                                                    if controller.dPadHorizontal == DPadRight then
                                                        (baseGravity + player.vy) * tempProperties.wallFriction
                                                    else
                                                        min (baseGravity + player.vy) earlyJumpTerminationVelocity

                                _ ->
                                    case horizontalDisplacement of
                                        Nothing ->
                                            baseGravity + player.vy

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection _ ->
                                                    if controller.dPadHorizontal == DPadLeft then
                                                        (baseGravity + player.vy) * tempProperties.wallFriction
                                                    else
                                                        baseGravity + player.vy

                                                CollisionPositiveDirection _ ->
                                                    if controller.dPadHorizontal == DPadRight then
                                                        (baseGravity + player.vy) * tempProperties.wallFriction
                                                    else
                                                        baseGravity + player.vy

                        playerYVelCapped =
                            capPlayerVelocity maxFallSpeed playerYVelAfterGravity

                        playerYAfterLocationUpdate =
                            playerYVelCapped + player.y

                        ----- how much the player is overlapping with platforms vertically
                        verticalDisplacement =
                            getOverlappingGridSquareCoords ( playerXAfterDisplacement, playerYAfterLocationUpdate ) playerHitBoxSize platforms
                                |> List.filter (\coord -> Dict.member coord platforms)
                                |> List.map (\( _, y ) -> getDisplacement (getY playerHitBoxSize) playerYAfterLocationUpdate (getY platformSize) (toFloat y))
                                |> List.head

                        ( playerYAfterDisplacement, playerVYAfterDisplacement, collidedWithTopOfPlatform ) =
                            case verticalDisplacement of
                                Nothing ->
                                    ( playerYAfterLocationUpdate, playerYVelCapped, False )

                                Just collision ->
                                    case collision of
                                        CollisionNegativeDirection overlap ->
                                            ( playerYAfterLocationUpdate + overlap, fullStop, True )

                                        CollisionPositiveDirection overlap ->
                                            ( playerYAfterLocationUpdate - overlap, fullStop, False )

                        newPlayerState =
                            if collidedWithTopOfPlatform then
                                Just OnTheGround
                            else
                                Nothing
                    in
                        { player
                            | x = playerXAfterDisplacement
                            , y = playerYAfterDisplacement
                            , vx = playerVXAfterDisplacement
                            , vy = playerVYAfterDisplacement
                            , playerState = newPlayerState
                        }

                Just state ->
                    case state of
                        Dead ->
                            -- update the player with a dieing animation?
                            player

                        Dashing ->
                            let
                                playerXvelUpdated =
                                    playerXVelAfterFriction + dPadForce

                                playerXAfterLocationUpdate =
                                    playerXvelUpdated + player.x

                                ----- how much the player is overlapping with platforms horizontally
                                horizontalDisplacement =
                                    getOverlappingGridSquareCoords ( playerXAfterLocationUpdate, player.y ) playerHitBoxSize platforms
                                        |> List.filter (\coord -> Dict.member coord platforms)
                                        |> List.map (\( x, _ ) -> getDisplacement (getX playerHitBoxSize) playerXAfterLocationUpdate (getX platformSize) (toFloat x))
                                        |> List.head

                                ----- move out of collision
                                ( playerXAfterDisplacement, playerVXAfterDisplacement, horizontalCollisionHappened ) =
                                    case horizontalDisplacement of
                                        Nothing ->
                                            ( playerXAfterLocationUpdate, playerXvelUpdated, False )

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection overlap ->
                                                    ( playerXAfterLocationUpdate + overlap, fullStop, True )

                                                CollisionPositiveDirection overlap ->
                                                    ( playerXAfterLocationUpdate - overlap, fullStop, True )

                                playerYVelAfterGravity =
                                    baseGravity + player.vy

                                playerYVelFirstUpdate =
                                    case controller.jump of
                                        Pressed ->
                                            baseJumpVelocityY

                                        _ ->
                                            playerYVelAfterGravity

                                playerYVelCapped =
                                    capPlayerVelocity maxFallSpeed playerYVelFirstUpdate

                                playerYAfterLocationUpdate =
                                    playerYVelCapped + player.y

                                ----- how much the player is overlapping with platforms vertically
                                verticalDisplacement =
                                    getOverlappingGridSquareCoords ( playerXAfterDisplacement, playerYAfterLocationUpdate ) playerHitBoxSize platforms
                                        |> List.filter (\coord -> Dict.member coord platforms)
                                        |> List.map (\( _, y ) -> getDisplacement (getY playerHitBoxSize) playerYAfterLocationUpdate (getY platformSize) (toFloat y))
                                        |> List.head

                                ( playerYAfterDisplacement, playerVYAfterDisplacement, collidedWithTopOfPlatform ) =
                                    case verticalDisplacement of
                                        Nothing ->
                                            ( playerYAfterLocationUpdate, playerYVelCapped, False )

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection overlap ->
                                                    ( playerYAfterLocationUpdate + overlap, fullStop, True )

                                                CollisionPositiveDirection overlap ->
                                                    ( playerYAfterLocationUpdate - overlap, fullStop, False )

                                newPlayerState =
                                    if controller.jump == Pressed then
                                        Nothing
                                    else
                                        Just Dashing
                            in
                                { player
                                    | x = playerXAfterDisplacement
                                    , y = playerYAfterDisplacement
                                    , vx = playerVXAfterDisplacement
                                    , vy = playerVYAfterDisplacement
                                    , playerState = newPlayerState
                                }

                        OnTheGround ->
                            let
                                playerStartedDashing =
                                    controller.dash == Pressed && not (controller.jump == Pressed)

                                playerXvelUpdated =
                                    if playerStartedDashing then
                                        dashingVelocity
                                    else
                                        playerXVelAfterFriction + dPadForce

                                playerXAfterLocationUpdate =
                                    playerXvelUpdated + player.x

                                ----- how much the player is overlapping with platforms horizontally
                                horizontalDisplacement =
                                    getOverlappingGridSquareCoords ( playerXAfterLocationUpdate, player.y ) playerHitBoxSize platforms
                                        |> List.filter (\coord -> Dict.member coord platforms)
                                        |> List.map (\( x, _ ) -> getDisplacement (getX playerHitBoxSize) playerXAfterLocationUpdate (getX platformSize) (toFloat x))
                                        |> List.head

                                ----- move out of collision
                                ( playerXAfterDisplacement, playerVXAfterDisplacement, horizontalCollisionHappened ) =
                                    case horizontalDisplacement of
                                        Nothing ->
                                            ( playerXAfterLocationUpdate, playerXvelUpdated, False )

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection overlap ->
                                                    ( playerXAfterLocationUpdate + overlap, fullStop, True )

                                                CollisionPositiveDirection overlap ->
                                                    ( playerXAfterLocationUpdate - overlap, fullStop, True )

                                playerYVelAfterGravity =
                                    baseGravity + player.vy

                                playerYVelFirstUpdate =
                                    case controller.jump of
                                        Pressed ->
                                            baseJumpVelocityY

                                        _ ->
                                            playerYVelAfterGravity

                                playerYVelCapped =
                                    capPlayerVelocity maxFallSpeed playerYVelFirstUpdate

                                playerYAfterLocationUpdate =
                                    playerYVelCapped + player.y

                                ----- how much the player is overlapping with platforms vertically
                                verticalDisplacement =
                                    getOverlappingGridSquareCoords ( playerXAfterDisplacement, playerYAfterLocationUpdate ) playerHitBoxSize platforms
                                        |> List.filter (\coord -> Dict.member coord platforms)
                                        |> List.map (\( _, y ) -> getDisplacement (getY playerHitBoxSize) playerYAfterLocationUpdate (getY platformSize) (toFloat y))
                                        |> List.head

                                ( playerYAfterDisplacement, playerVYAfterDisplacement, collidedWithTopOfPlatform ) =
                                    case verticalDisplacement of
                                        Nothing ->
                                            ( playerYAfterLocationUpdate, playerYVelCapped, False )

                                        Just collision ->
                                            case collision of
                                                CollisionNegativeDirection overlap ->
                                                    ( playerYAfterLocationUpdate + overlap, fullStop, True )

                                                CollisionPositiveDirection overlap ->
                                                    ( playerYAfterLocationUpdate - overlap, fullStop, False )

                                updatedPlayerState =
                                    if playerStartedDashing && not horizontalCollisionHappened then
                                        Just Dashing
                                    else if collidedWithTopOfPlatform then
                                        Just OnTheGround
                                    else
                                        Nothing
                            in
                                { player
                                    | x = playerXAfterDisplacement
                                    , y = playerYAfterDisplacement
                                    , vx = playerVXAfterDisplacement
                                    , vy = playerVYAfterDisplacement
                                    , playerState = updatedPlayerState
                                }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub ( updatedPlayer.x, updatedPlayer.y ) ( -100, -100 )) state.camera
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
