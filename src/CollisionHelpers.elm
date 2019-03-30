module CollisionHelpers exposing
    ( CollisionDirection(..)
    , getCollisionWithDisplacement
    , getGridCoordinatesPlayerIsOverlapping
    )

import Coordinates
import Dict
import GamePlatform
import GameTypes


getGridCoordinatesPlayerIsOverlapping : Float -> Float -> GameTypes.IntVector -> Dict.Dict GameTypes.IntVector GamePlatform.Platform -> List GameTypes.IntVector
getGridCoordinatesPlayerIsOverlapping x y size platforms =
    ------ only works for squares
    let
        ( width, height ) =
            size

        leftSide =
            round x

        rightSide =
            round (x + toFloat width - 1)

        bottom =
            round y

        top =
            round (y + toFloat height - 1)

        topLeft =
            ( leftSide, top )

        topRight =
            ( rightSide, top )

        bottomLeft =
            ( leftSide, bottom )

        bottomRight =
            ( rightSide, bottom )

        box4Corners =
            [ topLeft
            , topRight
            , bottomLeft
            , bottomRight
            ]
    in
    List.map (\location -> Coordinates.locationToGridCoordinate <| GameTypes.vectorIntToFloat location) box4Corners


type CollisionDirection
    = CollisionPositiveDirection Float
    | CollisionNegativeDirection Float


getCollisionWithDisplacement : Int -> Float -> Int -> Float -> CollisionDirection
getCollisionWithDisplacement sizeOne positionOne sizeTwo positionTwo =
    let
        halfSizeOne =
            toFloat sizeOne / 2

        halfSizeTwo =
            toFloat sizeTwo / 2

        minDistanceBetweenCenters =
            halfSizeOne + halfSizeTwo

        distanceBetweenCenters =
            abs (positionOne - positionTwo)

        overlap =
            max (minDistanceBetweenCenters - distanceBetweenCenters) 0
    in
    case positionOne > positionTwo of
        True ->
            CollisionNegativeDirection overlap

        False ->
            CollisionPositiveDirection overlap
