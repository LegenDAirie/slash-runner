module Coordinates exposing
    ( calculateCanvasSize
    , convertMouseCoorToGameCoor
    , convertToGameUnits
    , gameSize
    , gridSquareSize
    , gridToPixelConversion
    , locationToGridCoordinate
    , pixelToGridConversion
    )

import Game.TwoD.Camera as Camera
import GameTypes exposing (IntVector, Vector, vectorFloatToInt)
import V2


gameSize : Vector
gameSize =
    ( 1280, 720 )


gridSquareSize : IntVector
gridSquareSize =
    ( 64, 64 )


locationToGridCoordinate : Vector -> IntVector
locationToGridCoordinate location =
    location
        |> pixelToGridConversion
        |> gridToPixelConversion
        |> vectorFloatToInt


calculateCanvasSize : Vector -> Vector
calculateCanvasSize ( width, height ) =
    let
        newWidth =
            min width (16 / 9 * height)

        newHeight =
            min height (9 / 16 * width)
    in
    ( newWidth, newHeight )


gridToPixelConversion : Vector -> Vector
gridToPixelConversion ( gridX, gridY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
    ( gridX * toFloat gridSquareInPixelsX, gridY * toFloat gridSquareInPixelsY )


pixelToGridConversion : Vector -> Vector
pixelToGridConversion ( pixelX, pixelY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
    ( toFloat (floor (pixelX / toFloat gridSquareInPixelsX)), toFloat (floor (pixelY / toFloat gridSquareInPixelsY)) )


convertMouseCoorToGameCoor : Camera.Camera -> Vector -> Vector
convertMouseCoorToGameCoor camera mouseLocation =
    mouseLocation
        |> offSetOrigin
        |> offSetByCamera camera
        |> vectorFlipY


vectorFlipY : Vector -> Vector
vectorFlipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Vector -> Vector -> Vector
convertToGameUnits canvasSize mouseLocation =
    V2.scale (Tuple.first gameSize / Tuple.first canvasSize) mouseLocation


offSetOrigin : Vector -> Vector
offSetOrigin location =
    gameSize
        |> V2.scale 0.5
        |> V2.sub location


offSetByCamera : Camera.Camera -> Vector -> Vector
offSetByCamera camera location =
    camera
        |> Camera.getPosition
        |> vectorFlipY
        |> V2.add location
