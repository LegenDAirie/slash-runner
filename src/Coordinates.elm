module Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits, gameSize, centerToBottomLeftLocationConverter, gridSquareSize, gridToPixelConversion)

import GameTypes exposing (Vector)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)


gameSize : Vector
gameSize =
    ( 1280, 720 )


gridSquareSize : Vector
gridSquareSize =
    ( 64, 64 )


gridToPixelConversion : Vector -> Vector
gridToPixelConversion ( gridX, gridY ) =
    let
        ( gridSquareInPixelsX, gridSquareInPixelsY ) =
            gridSquareSize
    in
        ( gridX * gridSquareInPixelsX, gridY * gridSquareInPixelsY )


convertTouchCoorToGameCoor : Camera -> Vector -> Vector
convertTouchCoorToGameCoor camera touchLocation =
    touchLocation
        |> offSetOrigin
        |> offSetByCamera camera
        |> flipY


flipY : Vector -> Vector
flipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Vector -> Vector -> Vector
convertToGameUnits canvasSize touchLocation =
    V2.scale (getX gameSize / getX canvasSize) touchLocation


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
        ( currentX - width / 2, currentY - height / 2 )
