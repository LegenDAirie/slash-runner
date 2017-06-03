module Enemy exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Vector, vectorDecoder, Player, PlayerState(..))
import Coordinates exposing (centerToBottomLeftLocationConverter, gridToPixelConversion)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Collision2D
import Forces exposing (gravity, maxVerticalSpeed, airResistance)
import GamePlatform exposing (Platform, PlatformType(..), platformSize)


type alias Enemy =
    { startingLocation : Vector
    , timeExisted : Int
    , size : Vector
    , movement : Movement
    , directionLeft : Bool
    }


type Movement
    = NoMovement
    | LinePath LineMovementSpec
    | Walk Vector


type alias LineMovementSpec =
    { startNode : Vector
    , endNode : Vector
    , currentLocation : Vector
    , speed : Float
    }


updateEnemies : Player -> List Platform -> List Enemy -> List Enemy
updateEnemies player platforms enemies =
    enemies
        |> List.map (updateEnemy platforms)
        |> List.filter (notCollidingWithPlayer player)


updateEnemy : List Platform -> Enemy -> Enemy
updateEnemy platforms enemy =
    let
        newTimeExisted =
            enemy.timeExisted + 1
    in
        case enemy.movement of
            NoMovement ->
                { enemy
                    | timeExisted = newTimeExisted
                }

            Walk currentLocation ->
                { enemy
                    | timeExisted = newTimeExisted
                    , movement = Walk (updateWalkingEnemyLocation enemy.directionLeft currentLocation enemy.size platforms)
                }

            LinePath linePathSpec ->
                { enemy
                    | timeExisted = newTimeExisted
                    , movement = LinePath { linePathSpec | currentLocation = updateLinePath newTimeExisted enemy.startingLocation linePathSpec }
                }


updateWalkingEnemyLocation : Bool -> Vector -> Vector -> List Platform -> Vector
updateWalkingEnemyLocation direction currentLocation size platforms =
    let
        velocity =
            case direction of
                True ->
                    ( -3, 0 )

                False ->
                    ( 3, 0 )

        ( newLocation, newVelocity ) =
            applyPhysics currentLocation velocity

        ( setEnemyLocation, sideCollidingWithPlatform, platformType ) =
            setByPlatform newLocation size platforms Nothing Normal
    in
        setEnemyLocation


updateLinePath : Int -> Vector -> LineMovementSpec -> Vector
updateLinePath timeExisted startingLocation linePathSpec =
    let
        { startNode, endNode } =
            linePathSpec

        -- _ =
        --     Debug.log "startNode" startNode
        --
        -- _ =
        --     Debug.log "endNode" endNode
        halfWayPoint =
            V2.sub endNode startNode
                |> V2.divideBy 2

        -- _ =
        --     Debug.log "halfWayPoint" halfWayPoint
        -- _ =
        --     Debug.log "time occupied" timeExisted
        -- halfDistance =
        --     (getX endNode - getX startNode) / 2
        newLocation =
            halfWayPoint
                |> V2.scale (sin (toFloat timeExisted * 0.017))
                |> V2.add startNode
                |> V2.add halfWayPoint

        -- _ =
        --     Debug.log "x" (sin timeExisted)
    in
        newLocation



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


setByPlatform : Vector -> Vector -> List Platform -> Maybe Collision2D.Side -> PlatformType -> ( Vector, Maybe Collision2D.Side, PlatformType )
setByPlatform location size platforms lastSide platformType =
    case platforms of
        [] ->
            ( location, lastSide, platformType )

        platform :: rest ->
            case isCollidingWithPlatform location size platform of
                Just side ->
                    let
                        newSide =
                            calculateNewSide location platform side
                    in
                        case platform.platformType of
                            Normal ->
                                setByPlatform (setEntity location size platform newSide) size rest (Just newSide) platformType

                            Dangerous ->
                                setByPlatform (setEntity location size platform newSide) size rest (Just newSide) Dangerous

                Nothing ->
                    setByPlatform location size rest lastSide platformType


isCollidingWithPlatform : Vector -> Vector -> Platform -> Maybe Collision2D.Side
isCollidingWithPlatform entityLocation entitySize platform =
    let
        ( x, y ) =
            entityLocation

        ( width, height ) =
            entitySize

        ( platformX, platformY ) =
            platform.location

        ( platformWidth, platformHeight ) =
            platformSize

        entityHitbox =
            Collision2D.rectangle x y width height

        platformHitbox =
            Collision2D.rectangle platformX platformY platformWidth platformHeight
    in
        Collision2D.rectangleSide entityHitbox platformHitbox


calculateNewSide : Vector -> Platform -> Collision2D.Side -> Collision2D.Side
calculateNewSide entityLocation platform side =
    let
        ( x, y ) =
            entityLocation

        ( platformX, platformY ) =
            platform.location

        ( platformWidth, platformHeight ) =
            platformSize
    in
        case side of
            Collision2D.Top ->
                Collision2D.Top

            Collision2D.Bottom ->
                Collision2D.Bottom

            Collision2D.Right ->
                if y > platformY + platformHeight / 2 then
                    Collision2D.Bottom
                else
                    Collision2D.Right

            Collision2D.Left ->
                if y > platformY + platformHeight / 2 then
                    Collision2D.Bottom
                else
                    Collision2D.Left


setEntity : Vector -> Vector -> Platform -> Collision2D.Side -> Vector
setEntity entityLocation entitySize platform side =
    let
        ( x, y ) =
            entityLocation

        ( entityWidth, entityHeight ) =
            entitySize

        ( platformX, platformY ) =
            platform.location

        ( platformWidth, platformHeight ) =
            platformSize

        minVerticalDistanceApart =
            entityHeight / 2 + platformHeight / 2

        minHorizontalDistanceApart =
            entityWidth / 2 + platformWidth / 2
    in
        case side of
            Collision2D.Top ->
                ( x, platformY - minVerticalDistanceApart )

            Collision2D.Bottom ->
                ( x, platformY + minVerticalDistanceApart )

            Collision2D.Right ->
                ( platformX - minHorizontalDistanceApart, y )

            Collision2D.Left ->
                ( platformX + minHorizontalDistanceApart, y )



----------------------------------------------------------------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------
-------------------------------------------------------------------------


maxWalkingVelocity : Float
maxWalkingVelocity =
    6


capHorizontalVelocity : Float -> Vector -> Vector
capHorizontalVelocity maxSpeed ( x, y ) =
    if x > maxSpeed then
        ( maxSpeed, y )
    else if x < -maxSpeed then
        ( -maxSpeed, y )
    else
        ( x, y )


capVerticalVelocity : Float -> Vector -> Vector
capVerticalVelocity maxSpeed ( x, y ) =
    if y < -maxSpeed then
        ( x, -maxSpeed )
    else
        ( x, y )


applyPhysics : Vector -> Vector -> ( Vector, Vector )
applyPhysics location velocity =
    let
        newVelocity =
            velocity
                |> (\( x, y ) -> ( x * airResistance, y ))
                |> V2.add gravity
                |> capHorizontalVelocity maxWalkingVelocity
                |> capVerticalVelocity maxVerticalSpeed

        newLocation =
            newVelocity
                |> V2.add location
    in
        ( newLocation, newVelocity )



-------------------------------------------------------------------------


notCollidingWithPlayer : Player -> Enemy -> Bool
notCollidingWithPlayer player enemy =
    let
        ( x, y ) =
            case enemy.movement of
                NoMovement ->
                    enemy.startingLocation

                Walk currentLocation ->
                    currentLocation

                LinePath { currentLocation } ->
                    currentLocation

        ( width, height ) =
            enemy.size

        ( playerX, playerY ) =
            player.location

        ( playerWidth, playerHeight ) =
            player.size

        enemyHitbox =
            Collision2D.rectangle x y width height

        playerHitbox =
            Collision2D.rectangle playerX playerY playerWidth playerHeight

        collision =
            Collision2D.rectangleSide enemyHitbox playerHitbox
    in
        case collision of
            Nothing ->
                True

            Just side ->
                case side of
                    Collision2D.Top ->
                        False

                    Collision2D.Bottom ->
                        dashedIntoByPlayer player.playerState

                    Collision2D.Right ->
                        dashedIntoByPlayer player.playerState

                    Collision2D.Left ->
                        dashedIntoByPlayer player.playerState


dashedIntoByPlayer : PlayerState -> Bool
dashedIntoByPlayer playerState =
    case playerState of
        Running ->
            True

        Jumping jumpForce ->
            True

        Falling ->
            True

        Dashing framesDashing ->
            False

        DashRecovery ( framesRecovering, onGround ) ->
            True

        OnPlatform ( framesOnPlatform, platformOnRight ) ->
            True

        HitStun framesHitStuned ->
            True


renderEnemy : Enemy -> List Renderable
renderEnemy enemy =
    let
        x =
            getX enemy.startingLocation

        y =
            getY enemy.startingLocation

        ( location, color ) =
            case enemy.movement of
                NoMovement ->
                    ( enemy.startingLocation, Color.red )

                Walk currentLocation ->
                    ( currentLocation, Color.purple )

                LinePath linePathSpec ->
                    ( linePathSpec.currentLocation, Color.orange )

        enemyRenderable =
            Render.shape
                Render.rectangle
                { color = color
                , position = centerToBottomLeftLocationConverter location enemy.size
                , size = enemy.size
                }

        linePathNodesRenderable =
            case enemy.movement of
                NoMovement ->
                    []

                Walk currentLocation ->
                    []

                LinePath { startNode, endNode } ->
                    let
                        startNodeRenderable =
                            renderLinePathNode startNode

                        endNodeRenderable =
                            renderLinePathNode endNode
                    in
                        [ startNodeRenderable, endNodeRenderable ]
    in
        List.concat
            [ [ enemyRenderable ]
            , linePathNodesRenderable
            ]


renderLinePathNode : Vector -> Renderable
renderLinePathNode location =
    Render.shape Render.circle
        { color = Color.lightBrown
        , position = centerToBottomLeftLocationConverter location ( 64, 64 )
        , size = ( 16, 16 )
        }


gridToPixelEnemyConvert : Enemy -> Enemy
gridToPixelEnemyConvert enemy =
    { enemy
        | startingLocation = gridToPixelConversion enemy.startingLocation
        , movement = gridToPixelMovementConvert enemy.movement
    }


gridToPixelMovementConvert : Movement -> Movement
gridToPixelMovementConvert movement =
    case movement of
        NoMovement ->
            NoMovement

        Walk currentLocation ->
            Walk currentLocation

        LinePath lineMovementSpec ->
            let
                newLineMovementSpec =
                    { lineMovementSpec
                        | startNode = gridToPixelConversion lineMovementSpec.startNode
                        , endNode = gridToPixelConversion lineMovementSpec.endNode
                    }
            in
                LinePath newLineMovementSpec


enemyDecoder : Decoder Enemy
enemyDecoder =
    decode Enemy
        |> required "location" vectorDecoder
        |> hardcoded 0
        |> hardcoded ( 64, 64 )
        |> required "movement" movementDecoder
        |> hardcoded True


movementDecoder : Decoder Movement
movementDecoder =
    Json.Decode.string
        |> Json.Decode.andThen stringToMovementType


stringToMovementType : String -> Decoder Movement
stringToMovementType movement =
    case movement of
        "NoMovement" ->
            Json.Decode.succeed NoMovement

        "Walk" ->
            Json.Decode.map Walk vectorDecoder

        "LinePath" ->
            Json.Decode.map LinePath decodeLinePathMovementSpec

        _ ->
            Json.Decode.succeed NoMovement


decodeLinePathMovementSpec : Decoder LineMovementSpec
decodeLinePathMovementSpec =
    decode LineMovementSpec
        |> required "startNode" vectorDecoder
        |> required "endNode" vectorDecoder
        |> required "startingLocation" vectorDecoder
        |> required "speed" Json.Decode.float
