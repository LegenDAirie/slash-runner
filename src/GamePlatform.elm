module GamePlatform
    exposing
        ( Platform
        , PlatformType(Normal, Dangerous)
        , renderPlatform
        , platformSize
        , platformWithLocationsDecoder
        )

import GameTypes exposing (Vector, IntVector, gridCoordinateDecoder, gridCoordToVector)
import Coordinates exposing (gridSquareSize)
import Game.TwoD.Render as Render exposing (Renderable)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Color exposing (Color)


type alias Platform =
    { platformType : PlatformType
    }


type PlatformType
    = Normal
    | Dangerous


platformSize : IntVector
platformSize =
    gridSquareSize


renderPlatform : Color -> IntVector -> Renderable
renderPlatform color location =
    let
        ( x, y ) =
            location
    in
        Render.shape
            Render.rectangle
            { position = ( toFloat x, toFloat y )
            , size = gridCoordToVector platformSize
            , color = color
            }


platformWithLocationsDecoder : Decoder ( IntVector, Platform )
platformWithLocationsDecoder =
    decode (,)
        |> required "location" gridCoordinateDecoder
        |> required "platform" platformDecoder


platformDecoder : Decoder Platform
platformDecoder =
    decode Platform
        |> required "platformType" platformTypeDecoder


platformTypeDecoder : Decoder PlatformType
platformTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen stringToPlatformType


stringToPlatformType : String -> Decoder PlatformType
stringToPlatformType platformType =
    case platformType of
        "Dangerous" ->
            Json.Decode.succeed Dangerous

        _ ->
            Json.Decode.succeed Normal
