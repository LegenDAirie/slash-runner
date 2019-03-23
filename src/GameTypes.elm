module GameTypes exposing
    ( IntVector
    , Player
    , PlayerState(..)
    , TempProperties
    , Vector
    , intVectorDecoder
    , vectorDecoder
    , vectorFloatToInt
    , vectorIntToFloat
    )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Tuple


type alias Vector =
    ( Float, Float )


type alias IntVector =
    ( Int, Int )


vectorFloatToInt : Vector -> IntVector
vectorFloatToInt ( x, y ) =
    ( round x, round y )


vectorIntToFloat : IntVector -> Vector
vectorIntToFloat ( x, y ) =
    ( toFloat x, toFloat y )


vectorDecoder : Decoder Vector
vectorDecoder =
    Json.Decode.succeed Tuple.pair
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


intVectorDecoder : Decoder IntVector
intVectorDecoder =
    Json.Decode.succeed Tuple.pair
        |> required "x" Json.Decode.int
        |> required "y" Json.Decode.int


type alias Player =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , playerState : PlayerState
    }


type PlayerState
    = OnTheGround Int
    | GroundDash Int
    | InTheAir Int
    | AirDash Int
    | Falling Int



-------------------------------------
-- stupid temp TempProperties
-----------------------------


type alias TempProperties =
    -- that jump feel
    { framesToApex : Int
    , maxJumpHeight : Float
    , minJumpHeight : Float
    , maxWallSlideSpeed : Float
    , maxWalkingSpeed : Float
    , maxRunningSpeed : Float
    , maxDashingSpeed : Float
    , dPadAcceleration : Float
    , dashDuration : Int
    , buttonPressWindow : Int
    }
