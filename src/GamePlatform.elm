module GamePlatform exposing
    ( Platform
    , PlatformType(..)
    , platformSize
    , platformWithLocationsDecoder
    , renderPlatform
    )

import Color exposing (Color)
import Game.TwoD.Render as Render exposing (Renderable)
import GameTypes exposing (IntVector, Vector, intVectorDecoder, vectorIntToFloat)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Platform =
    { platformType : PlatformType
    }


type PlatformType
    = Normal
    | Dangerous


platformSize : IntVector
platformSize =
    ( 64, 64 )


renderPlatform : Color -> IntVector -> Renderable
renderPlatform color location =
    let
        ( x, y ) =
            location
    in
    Render.shape
        Render.rectangle
        { position = ( toFloat x, toFloat y )
        , size = vectorIntToFloat platformSize
        , color = color
        }


platformWithLocationsDecoder : Decoder ( IntVector, Platform )
platformWithLocationsDecoder =
    Json.Decode.succeed Tuple.pair
        |> required "location" intVectorDecoder
        |> required "platform" platformDecoder


platformDecoder : Decoder Platform
platformDecoder =
    Json.Decode.succeed Platform
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
