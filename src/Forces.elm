module Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)

import GameTypes exposing (Vector)


speedCap : Float
speedCap =
    10


blastForce : Float
blastForce =
    20


resistance : Float
resistance =
    0.98


gravity : Vector
gravity =
    ( 0, -5 )


controllerLeftForce : Vector
controllerLeftForce =
    ( -50, 0 )


controllerRightForce : Vector
controllerRightForce =
    ( 50, 0 )
