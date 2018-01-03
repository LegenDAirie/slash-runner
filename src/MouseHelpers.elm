module MouseHelpers exposing (mouseToGridInPixels)

import GamePlatform exposing (platformSize)
import Coordinates exposing (gameSize, convertTouchCoorToGameCoor, convertToGameUnits, pixelToGridConversion, gridToPixelConversion)
import Game.TwoD.Camera as Camera exposing (Camera)
import GameTypes exposing (Vector)


mouseToGridInPixels : Vector -> Camera -> Vector -> Vector
mouseToGridInPixels canvasSize camera mousePosition =
    let
        ( width, height ) =
            platformSize

        newPosition =
            mousePosition
                |> convertToGameUnits canvasSize
                |> convertTouchCoorToGameCoor camera
                |> (\( x, y ) -> ( x + width / 2, y + height / 2 ))
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
        newPosition
