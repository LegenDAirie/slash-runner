module V2 exposing (IntVector, Vector2, add, divideBy, flip, intVectorDecoder, scale, sub, vectorDecoder, vectorFloatToInt, vectorIntToFloat)

------------------------------------------------------------------------
-- This file is suuuuuuuper temporary!!!!!!!1
-- It's only to assist with upgrading from Elm 0.18 to 0.19
------------------------------------------------------------------------

import Json.Decode
import Json.Decode.Pipeline


type alias IntVector =
    ( Int, Int )


vectorFloatToInt : Vector2 -> IntVector
vectorFloatToInt ( x, y ) =
    ( round x, round y )


vectorIntToFloat : IntVector -> Vector2
vectorIntToFloat ( x, y ) =
    ( toFloat x, toFloat y )


xyRecordToVector : { item | x : Float, y : Float } -> ( Float, Float )
xyRecordToVector { x, y } =
    ( x, y )


vectorDecoder : Json.Decode.Decoder Vector2
vectorDecoder =
    Json.Decode.succeed Tuple.pair
        |> Json.Decode.Pipeline.required "x" Json.Decode.float
        |> Json.Decode.Pipeline.required "y" Json.Decode.float


intVectorDecoder : Json.Decode.Decoder IntVector
intVectorDecoder =
    Json.Decode.succeed Tuple.pair
        |> Json.Decode.Pipeline.required "x" Json.Decode.int
        |> Json.Decode.Pipeline.required "y" Json.Decode.int


type alias Vector2 =
    ( Float, Float )


add : Vector2 -> Vector2 -> Vector2
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


divideBy : Float -> Vector2 -> Vector2
divideBy a ( x, y ) =
    ( x / a, y / a )


scale : Float -> Vector2 -> Vector2
scale a ( x, y ) =
    ( a * x, a * y )


sub : Vector2 -> Vector2 -> Vector2
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b
