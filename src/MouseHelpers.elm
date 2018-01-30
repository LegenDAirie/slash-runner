module MouseHelpers exposing (mouseToGridInPixels)

import GamePlatform exposing (platformSize)
import Coordinates exposing (convertMouseCoorToGameCoor, convertToGameUnits, pixelToGridConversion, gridToPixelConversion, calculateCanvasSize)
import Game.TwoD.Camera as Camera exposing (Camera)
import GameTypes exposing (Vector, IntVector, vectorFloatToInt)


mouseToGridInPixels : Vector -> Camera -> Vector -> IntVector
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
            case (windowHeight > (windowWidth * 9 / 16)) of
                True ->
                    0

                False ->
                    (windowHeight - canvasHeight) / 2

        newPosition =
            mousePosition
                |> (\( x, y ) -> ( x - xOffset, y - yOffset ))
                |> convertToGameUnits canvasSize
                |> convertMouseCoorToGameCoor camera
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
        vectorFloatToInt newPosition
