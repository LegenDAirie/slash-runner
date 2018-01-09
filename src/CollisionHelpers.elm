module CollisionHelpers exposing (moveOutOfCollision, getSideCollidingWithEnemies)

import GameTypes exposing (Vector)
import GamePlatform exposing (Platform, PlatformType(..), platformSize)
import Enemy exposing (Enemy, Movement(..))
import Collision2D


------------------------------------------------------------------
-- collision with enemies
------------------------------------------------------------------
------------------------------
-- main function
------------------------------


getSideCollidingWithEnemies : Vector -> Vector -> List Enemy -> Maybe Collision2D.Side -> Maybe Collision2D.Side
getSideCollidingWithEnemies location size enemies side =
    case enemies of
        [] ->
            side

        enemy :: rest ->
            case sideCollidingWithEnemy location size enemy of
                Just side ->
                    getSideCollidingWithEnemies location size rest (Just side)

                Nothing ->
                    getSideCollidingWithEnemies location size rest side



------------------------------
-- helper
------------------------------


sideCollidingWithEnemy : Vector -> Vector -> Enemy -> Maybe Collision2D.Side
sideCollidingWithEnemy entityLocation entitySize enemy =
    let
        ( x, y ) =
            entityLocation

        ( width, height ) =
            entitySize

        ( enemyX, enemyY ) =
            case enemy.movement of
                NoMovement ->
                    enemy.startingLocation

                Walk currentLocation ->
                    currentLocation

                LinePath { currentLocation } ->
                    currentLocation

        ( enemyWidth, enemyHeight ) =
            enemy.size

        entityHitbox =
            Collision2D.rectangle x y width height

        enemyHitbox =
            Collision2D.rectangle enemyX enemyY enemyWidth enemyHeight
    in
        Collision2D.rectangleSide entityHitbox enemyHitbox



--------------------------------------------------------------------------------
-- collision with platforms
--------------------------------------------------------------------------------
------------------------------
-- main function
------------------------------


moveOutOfCollision : Vector -> Vector -> List Platform -> Maybe Collision2D.Side -> PlatformType -> ( Vector, Maybe Collision2D.Side, PlatformType )
moveOutOfCollision location size platforms lastSide platformType =
    case platforms of
        [] ->
            ( location, lastSide, platformType )

        platform :: rest ->
            case sideOfPlatformBeingCollidedWith location size platform of
                Just side ->
                    let
                        newSide =
                            calculateNewCollisionSide location platform side
                    in
                        case platform.platformType of
                            Normal ->
                                moveOutOfCollision (setEntityXY location size platform newSide) size rest (Just newSide) platformType

                            Dangerous ->
                                moveOutOfCollision (setEntityXY location size platform newSide) size rest (Just newSide) Dangerous

                Nothing ->
                    moveOutOfCollision location size rest lastSide platformType



------------------------------
-- helpers
------------------------------


sideOfPlatformBeingCollidedWith : Vector -> Vector -> Platform -> Maybe Collision2D.Side
sideOfPlatformBeingCollidedWith entityLocation entitySize platform =
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


calculateNewCollisionSide : Vector -> Platform -> Collision2D.Side -> Collision2D.Side
calculateNewCollisionSide entityLocation platform side =
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


setEntityXY : Vector -> Vector -> Platform -> Collision2D.Side -> Vector
setEntityXY entityLocation entitySize platform side =
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
