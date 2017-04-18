module Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance)

import GameTypes exposing (Vector)


speedCap : Float
speedCap =
    10


resistance : Float
resistance =
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
