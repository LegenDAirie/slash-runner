module Forces exposing (gravity, controllerLeftForce, controllerRightForce, maxVerticalSpeed, airResistance)

import GameTypes exposing (Vector)


maxVerticalSpeed : Float
maxVerticalSpeed =
    15


airResistance : Float
airResistance =
    0.9


gravity : Vector
gravity =
    ( 0, -5 )


controllerLeftForce : Vector
controllerLeftForce =
    ( -1, 0 )


controllerRightForce : Vector
controllerRightForce =
    ( 1, 0 )
