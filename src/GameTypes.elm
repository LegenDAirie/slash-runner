module GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorDecoder
        , intVectorDecoder
        , vectorIntToFloat
        , vectorFloatToInt
        , PlayerState(OnTheGround, GroundDash, InTheAir, AirDash, Falling)
        , TempProperties
        )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


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
    decode (,)
        |> required "x" Json.Decode.float
        |> required "y" Json.Decode.float


intVectorDecoder : Decoder IntVector
intVectorDecoder =
    decode (,)
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
