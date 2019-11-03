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


gameScreenSize : Float -> V2.Vector2
gameScreenSize scale =
    ( 1280 * scale, 720 * scale )


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


convertMouseCoorToGameCoor : V2.Vector2 -> Camera.Camera -> V2.Vector2 -> V2.Vector2
convertMouseCoorToGameCoor sizeOfGameScreen camera mouseLocation =
    mouseLocation
        |> offSetOrigin sizeOfGameScreen
        |> offSetByCamera camera
        |> vectorFlipY


vectorFlipY : V2.Vector2 -> V2.Vector2
vectorFlipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Float -> V2.Vector2 -> V2.Vector2 -> V2.Vector2
convertToGameUnits gameScreenWidth canvasSize mouseLocation =
    -- change canvasSize to canvasWidth
    V2.scale (gameScreenWidth / Tuple.first canvasSize) mouseLocation


offSetOrigin : V2.Vector2 -> V2.Vector2 -> V2.Vector2
offSetOrigin sizeOfGameScreen location =
    V2.scale 0.5 sizeOfGameScreen
        |> V2.sub location


offSetByCamera : Camera.Camera -> V2.Vector2 -> V2.Vector2
offSetByCamera camera location =
    camera
        |> Camera.getPosition
        |> vectorFlipY
        |> V2.add location
