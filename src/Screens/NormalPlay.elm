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

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import GameTypes exposing (Vector, GridCoordinate, Player)
import Coordinates exposing (gameSize, gridToPixelConversion)
import Player exposing (renderPlayer)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Controller
    exposing
        ( ControllerState
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


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : Dict GridCoordinate Platform
    , camera : Camera
    , resources : Resources
    }


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0
        , permanentEnemies = []
        , enemies = []
        , platforms = Dict.empty
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        }


createLevel : LevelData -> NormalPlayState
createLevel levelData =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) ( 64, 64 ) 0
        , platforms = levelData.platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        }


type alias LevelData =
    { platforms : Dict GridCoordinate Platform
    }


getAcceleration : DPad -> Vector
getAcceleration dPad =
    case dPad of
        Up ->
            ( 0, 0.1 )

        UpRight ->
            ( 0.1, 0.1 )

        Right ->
            ( 0.1, 0 )

        DownRight ->
            ( 0.1, -0.1 )

        Down ->
            ( 0, -0.1 )

        DownLeft ->
            ( -0.1, -0.1 )

        Left ->
            ( -0.1, 0 )

        UpLeft ->
            ( -0.1, 0.1 )

        NoDirection ->
            ( 0, 0 )


updateNormalPlay : ControllerState -> NormalPlayState -> NormalPlayState
updateNormalPlay controllerState state =
    let
        { player, platforms } =
            state

        acceleration =
            getAcceleration controllerState.dPad

        velocityAfterAcceleration =
            V2.add player.velocity acceleration

        locationAfterMovement =
            V2.add player.location velocityAfterAcceleration

        -- ( locationAfterCollision, velocityAfterCollision ) =
        --     calculateLocationAndVelocityFromCollision locationAfterMovement velocityAfterAcceleration player.size platforms
        -- updatedPlayer =
        --     { player
        --         | location = locationAfterCollision
        --         , velocity = velocityAfterCollision
        --     }
        updatedPlayer =
            { player
                | location = locationAfterMovement
                , velocity = velocityAfterAcceleration
            }
    in
        { state
            | camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , player = updatedPlayer
        }



-- calculateLocationAndVelocityFromCollision : Vector -> Vector -> Vector -> Dict GridCoordinate Platform -> ( Vector, Vector )
-- calculateLocationAndVelocityFromCollision playerLocation playerVelocity playerSize platforms =
--     let
--         topLeftTile =
--         topRightTile =
--         bottomLeftTile =
--         bottomRightTile =
--
--         -- get overlapping tiles (should be 4)
--         -- calculate which ones are walls, floor or neither
--         -- put the ones that are walls and floors into a list
--         -- for each one displaced location (for walls x first, for floors y first)
--         -- return final displacedLocation
--
--
--         displacementVector =
--             getCollisionDisplacementVector playerLocation playerSize platform.location ( 64, 64 )
--
--         disPlacedLocation =
--             V2.add playerLocation displacementVector
--
--         velocityAfterCollision =
--             getVelocityAfterCollision playerVelocity displacementVector
--     in
--         calculateLocationAndVelocityFromCollision disPlacedLocation velocityAfterCollision playerSize rest


getCollisionDisplacementVector : Vector -> Vector -> Vector -> Vector -> Vector
getCollisionDisplacementVector boxOneXY boxOneWH boxTwoXY boxTwoWH =
    let
        ( boxOneHalfWidth, boxOneHalfHeight ) =
            V2.divideBy 2 boxOneWH

        ( boxTwoHalfWidth, boxTwoHalfHeight ) =
            V2.divideBy 2 boxTwoWH

        verticalDistanceBetweenCenters =
            abs (getY boxOneXY - getY boxTwoXY)

        minnimumVerticalDistanceBetweenCenters =
            (boxOneHalfHeight + boxTwoHalfHeight)

        overlappingVertically =
            verticalDistanceBetweenCenters < minnimumVerticalDistanceBetweenCenters

        horizontalDistanceBetweenCenters =
            abs (getX boxOneXY - getX boxTwoXY)

        minnimumHorizontalDistanceBetweenCenters =
            (boxOneHalfWidth + boxTwoHalfWidth)

        overlappingHorizontally =
            horizontalDistanceBetweenCenters < minnimumHorizontalDistanceBetweenCenters

        colliding =
            overlappingVertically && overlappingHorizontally

        --------------------------------------------------------------------------------------
        boxOneIsAboveBoxTwo =
            getY boxOneXY > getY boxTwoXY

        amountOverlappingVertically =
            max (minnimumVerticalDistanceBetweenCenters - verticalDistanceBetweenCenters) 0

        verticalDisplacement =
            case colliding of
                True ->
                    case boxOneIsAboveBoxTwo of
                        True ->
                            amountOverlappingVertically

                        False ->
                            -amountOverlappingVertically

                False ->
                    0

        boxOneIsRightOfBoxTwo =
            getX boxOneXY > getX boxTwoXY

        amountOverlappingHorizontally =
            max (minnimumHorizontalDistanceBetweenCenters - horizontalDistanceBetweenCenters) 0

        horizontalDisplacement =
            case colliding of
                True ->
                    case boxOneIsRightOfBoxTwo of
                        True ->
                            amountOverlappingHorizontally

                        False ->
                            -amountOverlappingHorizontally

                False ->
                    0
    in
        case abs verticalDisplacement < abs horizontalDisplacement of
            True ->
                ( 0, verticalDisplacement )

            False ->
                ( horizontalDisplacement, 0 )


getVelocityAfterCollision : Vector -> Vector -> Vector
getVelocityAfterCollision currentVelocity locationDisplacementVector =
    let
        ( displacementX, displacementY ) =
            locationDisplacementVector

        ( currentVelocityX, currentVelocityY ) =
            currentVelocity

        newVelocityX =
            if displacementX == 0 then
                currentVelocityX
            else
                0

        newVelocitY =
            if displacementY == 0 then
                currentVelocityY
            else
                0
    in
        ( newVelocityX, newVelocitY )


renderNormalPlay : NormalPlayState -> List Renderable
renderNormalPlay state =
    List.concat
        [ (List.map (\( gridCoordinate, platform ) -> renderPlatform gridCoordinate platform) (Dict.toList state.platforms))
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
