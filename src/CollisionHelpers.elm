module CollisionHelpers exposing (setByPlatform, getSideCollidingWithEnemies)

import GameTypes exposing (Vector)
import GamePlatform exposing (Platform, PlatformType(..), platformSize)
import Enemy exposing (Enemy)
import Collision2D


------------------------------------------------------------------
-- collision with enemies
------------------------------------------------------------------


isCollidingWithEnemy : Vector -> Vector -> Enemy -> Maybe Collision2D.Side
isCollidingWithEnemy entityLocation entitySize enemy =
    let
        ( x, y ) =
            entityLocation

        ( width, height ) =
            entitySize

        ( enemyX, enemyY ) =
            enemy.startingLocation

        ( enemyWidth, enemyHeight ) =
            enemy.size

        entityHitbox =
            Collision2D.rectangle x y width height

        enemyHitbox =
            Collision2D.rectangle enemyX enemyY enemyWidth enemyHeight
    in
        Collision2D.rectangleSide entityHitbox enemyHitbox


getSideCollidingWithEnemies : Vector -> Vector -> List Enemy -> Maybe Collision2D.Side -> Maybe Collision2D.Side
getSideCollidingWithEnemies location size enemies side =
    case enemies of
        [] ->
            side

        enemy :: rest ->
            case isCollidingWithEnemy location size enemy of
                Just side ->
                    getSideCollidingWithEnemies location size rest (Just side)

                Nothing ->
                    getSideCollidingWithEnemies location size rest side



------------------------------------------------------------------
-- collision with platforms
------------------------------------------------------------------


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
