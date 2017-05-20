module Enemy exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Vector, vectorDecoder, Player, PlayerState(..))
import Coordinates exposing (centerToBottomLeftLocationConverter, gridToPixelConversion)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Collision2D


type alias Enemy =
    { location : Vector
    , timeExisted : Int
    , size : Vector
    , movement : Movement
    , directionLeft : Bool
    }


type Movement
    = NoMovement
    | LinePath LineMovementSpec
    | Walk


type alias LineMovementSpec =
    { startNode : Vector
    , endNode : Vector
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
            , location = updateMovement newTimeExisted enemy.movement enemy.location
        }


updateMovement : Int -> Movement -> Vector -> Vector
updateMovement timeExisted movement location =
    case movement of
        NoMovement ->
            location

        Walk ->
            location

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


notCollidingWithPlayer : Player -> Enemy -> Bool
notCollidingWithPlayer player enemy =
    let
        ( x, y ) =
            enemy.location

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
            getX enemy.location

        y =
            getY enemy.location

        color =
            case enemy.movement of
                NoMovement ->
                    Color.red

                Walk ->
                    Color.purple

                LinePath linePathSpec ->
                    Color.orange

        enemyRenderable =
            Render.shape
                Render.rectangle
                { color = color
                , position = centerToBottomLeftLocationConverter enemy.location enemy.size
                , size = enemy.size
                }

        linePathNodesRenderable =
            case enemy.movement of
                NoMovement ->
                    []

                Walk ->
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
        | location = gridToPixelConversion enemy.location
        , movement = gridToPixelMovementConvert enemy.movement
    }


gridToPixelMovementConvert : Movement -> Movement
gridToPixelMovementConvert movement =
    case movement of
        NoMovement ->
            NoMovement

        Walk ->
            Walk

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
    Json.Decode.oneOf
        [ decodeWithNoData
        , decodeWithData
        ]


decodeWithNoData : Decoder Movement
decodeWithNoData =
    Json.Decode.string
        |> Json.Decode.andThen stringToMovementType


stringToMovementType : String -> Decoder Movement
stringToMovementType movement =
    case movement of
        "Walk" ->
            Json.Decode.succeed Walk

        "NoMovement" ->
            Json.Decode.succeed NoMovement

        _ ->
            Json.Decode.succeed NoMovement


decodeWithData : Decoder Movement
decodeWithData =
    Json.Decode.map LinePath decodeLinePathMovementSpec


decodeLinePathMovementSpec : Decoder LineMovementSpec
decodeLinePathMovementSpec =
    decode LineMovementSpec
        |> required "startNode" vectorDecoder
        |> required "endNode" vectorDecoder
        |> required "speed" Json.Decode.float
