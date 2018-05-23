module GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorDecoder
        , intVectorDecoder
        , vectorIntToFloat
        , vectorFloatToInt
        , PlayerState(Dashing, OnTheGround, RecoveringFromDash, InTheAir)
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
    = Dashing Int
    | RecoveringFromDash Int
    | OnTheGround Int
    | InTheAir Int



-------------------------------------
-- stupid temp TempProperties
-----------------------------


type alias TempProperties =
    { framesToApex : Int
    , maxJumpHeight : Float
    , minJumpHeight : Float
    , wallFriction : Float
    , maxWalkingSpeed : Float
    , maxRunningSpeed : Float
    , dPadAcceleration : Float
    , dashDuration : Int
    , dashRecoveryDuration : Int
    , buttonPressWindow : Int
    }
