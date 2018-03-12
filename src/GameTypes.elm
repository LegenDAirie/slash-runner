module GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , vectorDecoder
        , intVectorDecoder
        , vectorIntToFloat
        , vectorFloatToInt
        , intVectorAdd
        , PersistantPlayerState(Dead, Dashing)
        , PlayerStateThisFrame
            ( SlidingOnWall
            , OnTheGround
            , JumpingFalling
            )
        )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Vector =
    ( Float, Float )


type alias IntVector =
    ( Int, Int )


intVectorAdd : IntVector -> IntVector -> IntVector
intVectorAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


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
    , spriteSize : IntVector
    , hitBoxSize : IntVector
    , playerState : Maybe PersistantPlayerState
    }


type PersistantPlayerState
    = Dead
    | Dashing


type PlayerStateThisFrame
    = SlidingOnWall
    | OnTheGround
    | JumpingFalling
    | PersistantState PersistantPlayerState
