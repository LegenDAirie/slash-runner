module V2 exposing (add, divideBy, flip, scale, sub)

------------------------------------------------------------------------
-- This file is suuuuuuuper temporary!!!!!!!1
-- It's only to assist with upgrading from Elm 0.18 to 0.19
------------------------------------------------------------------------


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
