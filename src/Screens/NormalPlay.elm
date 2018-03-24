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
        { player = Player 0 0 0 0 ( 128, 128 ) ( 64, 64 ) Nothing
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
        { player = Player 0 0 0 0 ( 128, 128 ) ( 64, 64 ) Nothing
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

        groundFriction =
            if controller.dPadHorizontal == NoHorizontalDPad then
                -- Player is coming to a stop
                0.93
            else if controller.dash == Held then
                -- Player is running
                calculateFriction tempProperties.dPadAcceleration tempProperties.maxRunningSpeed
            else
                -- Player is walking
                calculateFriction tempProperties.dPadAcceleration tempProperties.maxWalkingSpeed

        -- ---- calc dpad forces
        dPadForce =
            case controller.dPadHorizontal of
                DPadRight ->
                    tempProperties.dPadAcceleration

                DPadLeft ->
                    -tempProperties.dPadAcceleration

                NoHorizontalDPad ->
                    0

        baseJumpVelocityY =
            calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight baseGravity

        wallJumpVelocityX =
            baseJumpVelocityY

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

        inJumpablePlayerState =
            case player.playerState of
                Just state ->
                    case state of
                        Dashing ->
                            True

                        OnTheGround ->
                            True

                        Dead ->
                            False

                Nothing ->
                    False

        isPlayerJumpingOffWall =
            if controller.jump == Pressed && not (player.playerState == Just OnTheGround) then
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
        getDirection number =
            if number < 0 then
                -1
            else
                1

        hasPlayerStartedADash =
            player.playerState == Just OnTheGround && controller.dash == Pressed

        playerXVelFirstUpdate =
            case isPlayerJumpingOffWall of
                Just direction ->
                    case direction of
                        Left ->
                            wallJumpVelocityX

                        Right ->
                            -wallJumpVelocityX

                Nothing ->
                    if hasPlayerStartedADash then
                        case controller.dPadHorizontal of
                            DPadLeft ->
                                -tempProperties.maxRunningSpeed

                            DPadRight ->
                                tempProperties.maxRunningSpeed

                            NoHorizontalDPad ->
                                getDirection player.vx * tempProperties.maxWalkingSpeed
                    else
                        player.vx + dPadForce

        playerXVelAfterFriction =
            playerXVelFirstUpdate * groundFriction

        playerXLocationFirstUpdate =
            player.x + playerXVelAfterFriction

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
                |> List.head

        --- displace out of collision
        --- update velocity accordingly
        ( playerXAfterDisplacement, playerVXAfterDisplacement ) =
            case horizontalDisplacements of
                Nothing ->
                    ( playerXLocationFirstUpdate, playerXVelAfterFriction )

                Just collision ->
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
                Nothing ->
                    False

                Just collision ->
                    case collision of
                        CollisionNegativeDirection _ ->
                            if controller.dPadHorizontal == DPadLeft then
                                True
                            else
                                False

                        CollisionPositiveDirection _ ->
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

        isPlayerJumping =
            case isPlayerJumpingOffWall of
                Nothing ->
                    inJumpablePlayerState

                Just _ ->
                    True

        playerYVelFirstUpdate =
            case controller.jump of
                Pressed ->
                    if isPlayerJumping && playerYVelAfterGravity >= -25 then
                        baseJumpVelocityY
                    else
                        playerYVelAfterGravity

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
            capPlayerVelocity maxFallSpeed playerYVelFirstUpdate

        playerYLocationFirstUpdate =
            player.y + playerYVelCapped

        overlappingGridSquareCoordsAgain =
            getOverlappingGridSquareCoord ( playerXAfterDisplacement, playerYLocationFirstUpdate ) player.hitBoxSize platforms
                |> Set.fromList
                |> Set.toList

        getVerticalDisplacement : Float -> CollisionDirection
        getVerticalDisplacement =
            getDisplacement (getY player.hitBoxSize) playerYLocationFirstUpdate (getY platformSize)

        verticalDisplacements =
            overlappingGridSquareCoordsAgain
                |> List.filter (\coord -> Dict.member coord platforms)
                |> List.map (\( x, y ) -> getVerticalDisplacement <| toFloat y)
                |> List.head

        ( playerYAfterDisplacement, playerVYAfterDisplacement, groundCollisionHappend ) =
            case verticalDisplacements of
                Nothing ->
                    ( playerYLocationFirstUpdate, playerYVelCapped, False )

                Just collision ->
                    case collision of
                        CollisionNegativeDirection overlap ->
                            ( playerYLocationFirstUpdate + overlap, fullStop, True )

                        CollisionPositiveDirection overlap ->
                            ( playerYLocationFirstUpdate - overlap, fullStop, True )

        newPlayerState =
            if groundCollisionHappend then
                Just OnTheGround
            else
                Nothing

        updatedPlayer =
            { player
                | x = playerXAfterDisplacement
                , y = playerYAfterDisplacement
                , vx = playerVXAfterDisplacement
                , vy = playerVYAfterDisplacement
                , playerState = newPlayerState
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
