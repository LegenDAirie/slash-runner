module Coordinates exposing
    ( calculateCanvasSize
    , convertMouseCoorToGameCoor
    , convertToGameUnits
    , gameScreenSize
    , gridSquareSize
    , gridToPixelConversion
    , locationToGridCoordinate
    , pixelToGridConversion
    )

import Game.TwoD.Camera as Camera
import V2


gameScreenSize : V2.Vector2
gameScreenSize =
    ( 1280, 720 )


gridSquareSize : V2.IntVector
gridSquareSize =
    ( 64, 64 )


locationToGridCoordinate : V2.Vector2 -> V2.IntVector
locationToGridCoordinate location =
    location
        |> pixelToGridConversion
        |> gridToPixelConversion
        |> V2.vectorFloatToInt


calculateCanvasSize : V2.Vector2 -> V2.Vector2
calculateCanvasSize ( width, height ) =
    let
        newWidth =
            min width (16 / 9 * height)

        newHeight =
            min height (9 / 16 * width)
    in
    ( newWidth, newHeight )


gridToPixelConversion : V2.Vector2 -> V2.Vector2
gridToPixelConversion ( gridX, gridY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
    ( gridX * toFloat gridSquareInPixelsX, gridY * toFloat gridSquareInPixelsY )


pixelToGridConversion : V2.Vector2 -> V2.Vector2
pixelToGridConversion ( pixelX, pixelY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
    ( toFloat (floor (pixelX / toFloat gridSquareInPixelsX)), toFloat (floor (pixelY / toFloat gridSquareInPixelsY)) )


convertMouseCoorToGameCoor : Camera.Camera -> V2.Vector2 -> V2.Vector2
convertMouseCoorToGameCoor camera mouseLocation =
    mouseLocation
        |> offSetOrigin
        |> offSetByCamera camera
        |> vectorFlipY


vectorFlipY : V2.Vector2 -> V2.Vector2
vectorFlipY ( x, y ) =
    ( x, -y )


convertToGameUnits : V2.Vector2 -> V2.Vector2 -> V2.Vector2
convertToGameUnits canvasSize mouseLocation =
    V2.scale (Tuple.first gameScreenSize / Tuple.first canvasSize) mouseLocation


offSetOrigin : V2.Vector2 -> V2.Vector2
offSetOrigin location =
    gameScreenSize
        |> V2.scale 0.5
        |> V2.sub location


offSetByCamera : Camera.Camera -> V2.Vector2 -> V2.Vector2
offSetByCamera camera location =
    camera
        |> Camera.getPosition
        |> vectorFlipY
        |> V2.add location
