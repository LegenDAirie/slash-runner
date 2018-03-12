module CollisionHelpers
    exposing
        ( getOverlappingGridSquareCoord
        , getDisplacement
        , CollisionDirection(CollisionNegativeDirection, CollisionPositiveDirection)
        )

import Dict exposing (Dict)
import GamePlatform exposing (Platform)
import Coordinates
    exposing
        ( pixelToGridConversion
        , gridToPixelConversion
        , gridSquareSize
        , locationToGridCoordinate
        )
import GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorFloatToInt
        , vectorIntToFloat
        , intVectorAdd
        )


getOverlappingGridSquareCoord : Vector -> IntVector -> Dict IntVector Platform -> List IntVector
getOverlappingGridSquareCoord location size platforms =
    ------ only works for squares
    let
        ( x, y ) =
            location

        ( width, height ) =
            size

        leftSide =
            floor x

        rightSide =
            ceiling (x + toFloat width - 1)

        bottom =
            floor y

        top =
            ceiling (y + toFloat height - 1)

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
        List.map (\location -> locationToGridCoordinate <| vectorIntToFloat location) box4Corners


type CollisionDirection
    = CollisionPositiveDirection Float
    | CollisionNegativeDirection Float


getDisplacement : Int -> Float -> Int -> Float -> CollisionDirection
getDisplacement widthOne positionOne widthTwo positionTwo =
    let
        halfWidthOne =
            toFloat widthOne / 2

        halfWidthTwo =
            toFloat widthTwo / 2

        minDistanceBetweenCenters =
            halfWidthOne + halfWidthTwo

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



-------------------- abstract this somehow plz --------------------------------------------------------------------


canDisplaceLeft : IntVector -> Dict IntVector Platform -> Bool
canDisplaceLeft platformLocation platforms =
    let
        ( width, height ) =
            gridSquareSize

        leftNeighbor =
            intVectorAdd platformLocation ( -width, 0 )
    in
        not (Dict.member leftNeighbor platforms)


canDisplaceRight : IntVector -> Dict IntVector Platform -> Bool
canDisplaceRight platformLocation platforms =
    let
        ( width, height ) =
            gridSquareSize

        rightNeighbor =
            intVectorAdd platformLocation ( width, 0 )
    in
        not (Dict.member rightNeighbor platforms)


canDisplaceUp : IntVector -> Dict IntVector Platform -> Bool
canDisplaceUp platformLocation platforms =
    let
        ( width, height ) =
            gridSquareSize

        aboveNeighbor =
            intVectorAdd platformLocation ( 0, height )
    in
        not (Dict.member aboveNeighbor platforms)


canDisplaceDown : IntVector -> Dict IntVector Platform -> Bool
canDisplaceDown platformLocation platforms =
    let
        ( width, height ) =
            gridSquareSize

        belowNeighbor =
            intVectorAdd platformLocation ( 0, -height )
    in
        not (Dict.member belowNeighbor platforms)
