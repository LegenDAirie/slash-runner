module MouseHelpers exposing (mouseToGridInPixels)

import Coordinates exposing (calculateCanvasSize, convertMouseCoorToGameCoor, convertToGameUnits, gridToPixelConversion, pixelToGridConversion)
import Game.TwoD.Camera as Camera exposing (Camera)
import GamePlatform
import GameTypes exposing (IntVector, Vector, vectorFloatToInt)


mouseToGridInPixels : Vector -> Camera -> Vector -> IntVector
mouseToGridInPixels windowSize camera mousePosition =
    let
        ( width, height ) =
            Coordinates.gridSquareSize

        canvasSize =
            calculateCanvasSize windowSize

        ( windowWidth, windowHeight ) =
            windowSize

        ( canvasWidth, canvasHeight ) =
            canvasSize

        xOffset =
            (windowWidth - canvasWidth) / 2

        yOffset =
            case windowHeight > (windowWidth * 9 / 16) of
                True ->
                    0

                False ->
                    (windowHeight - canvasHeight) / 2

        newPosition =
            mousePosition
                |> convertToGameUnits canvasSize
                |> convertMouseCoorToGameCoor camera
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
    vectorFloatToInt newPosition
