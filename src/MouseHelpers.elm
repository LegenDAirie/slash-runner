module MouseHelpers exposing (mouseToGridInPixels)

import GamePlatform exposing (platformSize)
import Coordinates exposing (convertMouseCoorToGameCoor, convertToGameUnits, pixelToGridConversion, gridToPixelConversion, calculateCanvasSize)
import Game.TwoD.Camera as Camera exposing (Camera)
import GameTypes exposing (Vector)


mouseToGridInPixels : Vector -> Camera -> Vector -> Vector
mouseToGridInPixels windowSize camera mousePosition =
    let
        ( width, height ) =
            platformSize

        canvasSize =
            calculateCanvasSize windowSize

        ( windowWidth, windowHeight ) =
            windowSize

        ( canvasWidth, canvasHeight ) =
            canvasSize

        xOffset =
            (windowWidth - canvasWidth) / 2

        yOffset =
            (windowHeight - canvasHeight) / 2

        newPosition =
            mousePosition
                |> (\( x, y ) -> ( x - xOffset, y - yOffset ))
                |> convertToGameUnits canvasSize
                |> convertMouseCoorToGameCoor camera
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
        newPosition
