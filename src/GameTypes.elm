module GameTypes exposing
    ( Player
    , PlayerState(..)
    , TempProperties
    )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Tuple
import V2


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
