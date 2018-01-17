module GameTypes
    exposing
        ( Vector
        , Player
        , vectorDecoder
        , GridCoordinate
        , gridCoordinateDecoder
        , vectorToGridCoordinate
        , gridCoordToVector
        )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Vector =
    ( Float, Float )


type alias GridCoordinate =
    ( Int, Int )


vectorToGridCoordinate : Vector -> GridCoordinate
vectorToGridCoordinate ( x, y ) =
    ( round x, round y )


gridCoordToVector : GridCoordinate -> Vector
gridCoordToVector ( x, y ) =
    ( toFloat x, toFloat y )


vectorDecoder : Decoder Vector
vectorDecoder =
    decode (,)
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


gridCoordinateDecoder : Decoder GridCoordinate
gridCoordinateDecoder =
    decode (,)
        |> required "x" Json.Decode.int
        |> required "y" Json.Decode.int


type alias Player =
    { location : Vector
    , velocity : Vector
    , size : Vector
    , framesSinceLastChain : Int
    }
