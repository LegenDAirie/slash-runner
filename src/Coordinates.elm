module Coordinates
    exposing
        ( convertMouseCoorToGameCoor
        , convertToGameUnits
        , gameSize
        , centerToBottomLeftLocationConverter
        , gridSquareSize
        , gridToPixelConversion
        , pixelToGridConversion
        , calculateCanvasSize
        )

import GameTypes exposing (Vector)
import Vector2 as V2 exposing (getX)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)


gameSize : Vector
gameSize =
    ( 1280, 720 )


gridSquareSize : Vector
gridSquareSize =
    ( 64, 64 )


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
        ( gridX * gridSquareInPixelsX, gridY * gridSquareInPixelsY )


pixelToGridConversion : Vector -> Vector
pixelToGridConversion ( pixelX, pixelY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
        ( toFloat (floor (pixelX / gridSquareInPixelsX)), toFloat (floor (pixelY / gridSquareInPixelsY)) )


convertMouseCoorToGameCoor : Camera -> Vector -> Vector
convertMouseCoorToGameCoor camera mouseLocation =
    mouseLocation
        |> offSetOrigin
        |> offSetByCamera camera
        |> flipY


flipY : Vector -> Vector
flipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Vector -> Vector -> Vector
convertToGameUnits canvasSize mouseLocation =
    V2.scale (getX gameSize / getX canvasSize) mouseLocation


offSetOrigin : Vector -> Vector
offSetOrigin touchLocation =
    gameSize
        |> V2.scale 0.5
        |> V2.sub touchLocation


offSetByCamera : Camera -> Vector -> Vector
offSetByCamera camera touchLocation =
    camera
        |> getPosition
        |> flipY
        |> V2.add touchLocation


centerToBottomLeftLocationConverter : Vector -> Vector -> Vector
centerToBottomLeftLocationConverter location size =
    let
        ( currentX, currentY ) =
            location

        ( width, height ) =
            size
    in
        ( currentX, currentY )
