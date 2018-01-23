module GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorDecoder
        , gridCoordinateDecoder
        , vectorToGridCoordinate
        , gridCoordToVector
        )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Vector =
    ( Float, Float )


type alias IntVector =
    ( Int, Int )


vectorToGridCoordinate : Vector -> IntVector
vectorToGridCoordinate ( x, y ) =
    ( round x, round y )


gridCoordToVector : IntVector -> Vector
gridCoordToVector ( x, y ) =
    ( toFloat x, toFloat y )


vectorDecoder : Decoder Vector
vectorDecoder =
    decode (,)
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


gridCoordinateDecoder : Decoder IntVector
gridCoordinateDecoder =
    decode (,)
        |> required "x" Json.Decode.int
        |> required "y" Json.Decode.int


type alias Player =
    { location : Vector
    , velocity : Vector
    , size : IntVector
    , framesSinceLastChain : Int
    }
