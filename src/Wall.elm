module Wall exposing (Wall, renderWall, wallSize, wallDecoder)

import Color
import GameTypes exposing (Vector, vectorDecoder)
import Coordinates exposing (centerToBottomLeftLocationConverter, gridSquareSize)
import Game.TwoD.Render as Render exposing (Renderable)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Wall =
    { location : Vector
    }


wallSize : Vector
wallSize =
    gridSquareSize


renderWall : Wall -> Renderable
renderWall wall =
    let
        ( x, y ) =
            wall.location
    in
        Render.rectangle
            { color = Color.charcoal
            , position = centerToBottomLeftLocationConverter wall.location wallSize
            , size = wallSize
            }


wallDecoder : Decoder Wall
wallDecoder =
    decode Wall
        |> required "location" vectorDecoder
