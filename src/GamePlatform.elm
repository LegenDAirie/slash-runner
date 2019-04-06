module GamePlatform exposing
    ( Platform(..)
    , platformWithLocationsDecoder
    , renderPlatform
    )

import Color exposing (Color)
import Coordinates
import Game.TwoD.Render as Render exposing (Renderable)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import V2



-- This doesn't need to be it's own file. Besides the Platform type,
-- it's all just decoders


type Platform
    = Normal
    | Dangerous


renderPlatform : Color -> V2.IntVector -> Renderable
renderPlatform color ( x, y ) =
    -- Render should probably not be in this file
    Render.shape
        Render.rectangle
        { position = ( toFloat x, toFloat y )
        , size = V2.vectorIntToFloat Coordinates.gridSquareSize
        , color = color
        }


platformWithLocationsDecoder : Decoder ( V2.IntVector, Platform )
platformWithLocationsDecoder =
    -- Hmmm so this is wierd...
    -- Either a platform should have both
    -- or
    -- a platform is separate from the collections that is the map
    Json.Decode.succeed Tuple.pair
        |> required "location" V2.intVectorDecoder
        |> required "platform" platformDecoder


platformDecoder : Decoder Platform
platformDecoder =
    Json.Decode.string
        |> Json.Decode.andThen stringToPlatform


stringToPlatform : String -> Decoder Platform
stringToPlatform platformType =
    case platformType of
        "Dangerous" ->
            Json.Decode.succeed Dangerous

        _ ->
            Json.Decode.succeed Normal
