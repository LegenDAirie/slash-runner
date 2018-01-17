module GamePlatform
    exposing
        ( Platform
        , PlatformType(Normal, Dangerous)
        , renderPlatform
        , platformSize
        , platformWithLocationsDecoder
        )

import GameTypes exposing (Vector, GridCoordinate, gridCoordinateDecoder)
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


platformSize : Vector
platformSize =
    gridSquareSize


renderPlatform : Color -> GridCoordinate -> Renderable
renderPlatform color location =
    let
        ( x, y ) =
            location
    in
        Render.shape
            Render.rectangle
            { position = ( toFloat x, toFloat y )
            , size = platformSize
            , color = color
            }


platformWithLocationsDecoder : Decoder ( GridCoordinate, Platform )
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
