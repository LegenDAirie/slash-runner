module GameTypes exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Vector =
    ( Float, Float )


vectorDecoder : Decoder Vector
vectorDecoder =
    decode (,)
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float
