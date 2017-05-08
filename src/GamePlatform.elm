module GamePlatform exposing (Platform, renderPlatform, platformSize, platformDecoder)

import Color
import GameTypes exposing (Vector, vectorDecoder)
import Coordinates exposing (centerToBottomLeftLocationConverter, gridSquareSize)
import Game.TwoD.Render as Render exposing (Renderable)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Platform =
    { location : Vector
    }


platformSize : Vector
platformSize =
    gridSquareSize


renderPlatform : Platform -> Renderable
renderPlatform platform =
    let
        ( x, y ) =
            platform.location
    in
        Render.shape
            Render.rectangle
            { color = Color.charcoal
            , position = centerToBottomLeftLocationConverter platform.location platformSize
            , size = platformSize
            }


platformDecoder : Decoder Platform
platformDecoder =
    decode Platform
        |> required "location" vectorDecoder
