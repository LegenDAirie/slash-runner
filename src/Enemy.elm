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


updateEnemies : Player -> List Enemy -> List Enemy
updateEnemies player enemies =
    enemies
        |> List.map updateEnemy
        |> List.filter (notCollidingWithPlayer player)


updateEnemy : Enemy -> Enemy
updateEnemy enemy =
    let
        newTimeExisted =
            enemy.timeExisted + 1
    in
        { enemy
            | timeExisted = newTimeExisted
            , startingLocation = updateMovement newTimeExisted enemy.movement enemy.startingLocation
        }


updateMovement : Int -> Movement -> Vector -> Vector
updateMovement timeExisted movement startingLocation =
    case movement of
        NoMovement ->
            startingLocation

        Walk currentLocation ->
            startingLocation

        LinePath { startNode, endNode, speed } ->
            let
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
            enemy.startingLocation

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

        color =
            case enemy.movement of
                NoMovement ->
                    Color.red

                Walk currentLocation ->
                    Color.purple

                LinePath linePathSpec ->
                    Color.orange

        enemyRenderable =
            Render.shape
                Render.rectangle
                { color = color
                , position = centerToBottomLeftLocationConverter enemy.startingLocation enemy.size
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



-- decodeWithNoData : Decoder Movement
-- decodeWithNoData =
-- Json.Decode.string
--     |> Json.Decode.andThen stringToMovementType


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



-- decodeWithData : Decoder Movement
-- decodeWithData =
--     case movement of


decodeLinePathMovementSpec : Decoder LineMovementSpec
decodeLinePathMovementSpec =
    decode LineMovementSpec
        |> required "startNode" vectorDecoder
        |> required "endNode" vectorDecoder
        |> required "startingLocation" vectorDecoder
        |> required "speed" Json.Decode.float
