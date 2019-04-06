module GameTypes exposing (TempProperties)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Tuple
import V2



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
