module MouseHelpers exposing (mouseToGridInPixels)

import Wall exposing (wallSize)
import Coordinates exposing (gameSize, convertTouchCoorToGameCoor, convertToGameUnits, pixelToGridConversion, gridToPixelConversion)
import Game.TwoD.Camera as Camera exposing (Camera)
import GameTypes exposing (Vector)


mouseToGridInPixels : Vector -> Camera -> Vector -> Vector
mouseToGridInPixels canvasSize camera location =
    let
        ( width, height ) =
            wallSize

        newPosition =
            location
                |> convertToGameUnits canvasSize
                |> convertTouchCoorToGameCoor camera
                |> (\( x, y ) -> ( x + width / 2, y + height / 2 ))
                |> pixelToGridConversion
                |> gridToPixelConversion
    in
        newPosition
