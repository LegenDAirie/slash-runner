module CollisionHelpers
    exposing
        ( getCollidingTiles
        , calculateLocationAndVelocityFromCollision
        , getCollisionDisplacementVector
        )

import Dict exposing (Dict)
import GamePlatform exposing (Platform)
import Coordinates exposing (pixelToGridConversion, gridToPixelConversion)
import Vector2 as V2 exposing (getX, getY)
import GameTypes exposing (Vector, GridCoordinate, Player, vectorToGridCoordinate, gridCoordToVector)


getCollidingTiles : GridCoordinate -> Vector -> Vector -> Dict GridCoordinate Platform -> List GridCoordinate
getCollidingTiles playerLocation playerVelocity playerSize platforms =
    let
        ( playerX, playerY ) =
            playerLocation

        ( playerWidth, playerHeight ) =
            playerSize

        playerLeftSide =
            playerX

        playerRightSide =
            playerX + (round playerWidth - 1)

        playerBottom =
            playerY

        playerTop =
            playerY + (round playerHeight - 1)

        playerTopLeft =
            ( playerLeftSide, playerTop )

        playerTopRight =
            ( playerRightSide, playerTop )

        playerBottomLeft =
            ( playerLeftSide, playerBottom )

        playerBottomRight =
            ( playerRightSide, playerBottom )

        topLeftTileCoord =
            playerTopLeft
                |> gridCoordToVector
                |> pixelToGridConversion
                |> gridToPixelConversion
                |> vectorToGridCoordinate

        topRightTileCoord =
            playerTopRight
                |> gridCoordToVector
                |> pixelToGridConversion
                |> gridToPixelConversion
                |> vectorToGridCoordinate

        bottomLeftTileCoord =
            playerBottomLeft
                |> gridCoordToVector
                |> pixelToGridConversion
                |> gridToPixelConversion
                |> vectorToGridCoordinate

        bottomRightTileCoord =
            playerBottomRight
                |> gridCoordToVector
                |> pixelToGridConversion
                |> gridToPixelConversion
                |> vectorToGridCoordinate
    in
        [ topLeftTileCoord, topRightTileCoord, bottomLeftTileCoord, bottomRightTileCoord ]


calculateLocationAndVelocityFromCollision : Vector -> Vector -> Vector -> List GridCoordinate -> Dict GridCoordinate Platform -> ( Vector, Vector )
calculateLocationAndVelocityFromCollision location velocity playerSize gridCoordinates platforms =
    case gridCoordinates of
        [] ->
            ( location, velocity )

        gridCoordinate :: rest ->
            case Dict.get gridCoordinate platforms of
                Nothing ->
                    calculateLocationAndVelocityFromCollision location velocity playerSize rest platforms

                Just _ ->
                    let
                        displacementVector =
                            getCollisionDisplacementVector location playerSize gridCoordinate ( 64, 64 )

                        newLocation =
                            V2.add location displacementVector

                        newVelocity =
                            getVelocityAfterCollision velocity displacementVector

                        _ =
                            Debug.log "location before collision " location

                        _ =
                            Debug.log "displacementVector" displacementVector

                        _ =
                            Debug.log "location after collision" newLocation
                    in
                        calculateLocationAndVelocityFromCollision newLocation newVelocity playerSize rest platforms


getCollisionDisplacementVector : Vector -> Vector -> GridCoordinate -> Vector -> Vector
getCollisionDisplacementVector boxOneXY boxOneWH boxTwoXY boxTwoWH =
    let
        ( boxOneHalfWidth, boxOneHalfHeight ) =
            V2.divideBy 2 boxOneWH

        ( boxTwoHalfWidth, boxTwoHalfHeight ) =
            V2.divideBy 2 boxTwoWH

        verticalDistanceBetweenCenters =
            abs (getY boxOneXY - toFloat (getY boxTwoXY))

        minnimumVerticalDistanceBetweenCenters =
            (boxOneHalfHeight + boxTwoHalfHeight)

        horizontalDistanceBetweenCenters =
            abs (getX boxOneXY - toFloat (getX boxTwoXY))

        minnimumHorizontalDistanceBetweenCenters =
            (boxOneHalfWidth + boxTwoHalfWidth)

        boxOneIsAboveBoxTwo =
            getY boxOneXY > toFloat (getY boxTwoXY)

        amountOverlappingVertically =
            max (minnimumVerticalDistanceBetweenCenters - verticalDistanceBetweenCenters) 0

        verticalDisplacement =
            case boxOneIsAboveBoxTwo of
                True ->
                    amountOverlappingVertically

                False ->
                    -amountOverlappingVertically

        boxOneIsRightOfBoxTwo =
            getX boxOneXY > toFloat (getX boxTwoXY)

        amountOverlappingHorizontally =
            max (minnimumHorizontalDistanceBetweenCenters - horizontalDistanceBetweenCenters) 0

        horizontalDisplacement =
            case boxOneIsRightOfBoxTwo of
                True ->
                    amountOverlappingHorizontally

                False ->
                    -amountOverlappingHorizontally
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
