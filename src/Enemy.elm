module Enemy exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Vector, vectorDecoder, Player, PlayerState(..))
import Coordinates exposing (centerToBottomLeftLocationConverter)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Collision2D


type alias Enemy =
    { location : Vector
    , timeExisted : Int
    , size : Vector
    , movement : Movement
    }


type Movement
    = NoMovement
    | LinePath LineMovementSpec


type alias LineMovementSpec =
    { startNode : Vector
    , endNode : Vector
    , startingDirectionLeftOrDown : Bool
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

        LinePath { startNode, endNode, startingDirectionLeftOrDown, speed } ->
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


renderEnemy : Enemy -> Renderable
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

                LinePath linePathSpec ->
                    Color.orange
    in
        Render.shape
            Render.rectangle
            { color = color
            , position = centerToBottomLeftLocationConverter enemy.location enemy.size
            , size = enemy.size
            }


enemyDecoder : Decoder Enemy
enemyDecoder =
    decode Enemy
        |> required "location" vectorDecoder
        |> hardcoded 0
        |> hardcoded ( 64, 64 )
        |> required "movement" movementDecoder


movementDecoder : Decoder Movement
movementDecoder =
    Json.Decode.oneOf
        [ decodeNoMovement
        , decodeLinePath
        ]


decodeNoMovement : Decoder Movement
decodeNoMovement =
    Json.Decode.null NoMovement


decodeLinePath : Decoder Movement
decodeLinePath =
    Json.Decode.map LinePath decodeLinePathMovementSpec


decodeLinePathMovementSpec : Decoder LineMovementSpec
decodeLinePathMovementSpec =
    decode LineMovementSpec
        |> required "startNode" vectorDecoder
        |> required "endNode" vectorDecoder
        |> required "startingDirectionLeftOrDown" Json.Decode.bool
        |> required "speed" Json.Decode.float
