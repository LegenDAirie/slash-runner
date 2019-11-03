module MouseHelpers exposing (mouseToGridInPixels)

import Coordinates exposing (calculateCanvasSize, convertMouseCoorToGameCoor, convertToGameUnits, gridToPixelConversion, pixelToGridConversion)
import Game.TwoD.Camera as Camera exposing (Camera)
import GamePlatform
import V2


mouseToGridInPixels : V2.Vector2 -> V2.Vector2 -> Camera -> V2.Vector2 -> V2.IntVector
mouseToGridInPixels sizeOfGameScreen windowSize camera mousePosition =
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
                |> convertToGameUnits (Tuple.first sizeOfGameScreen) canvasSize
                |> convertMouseCoorToGameCoor sizeOfGameScreen camera
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
    V2.vectorFloatToInt newPosition
