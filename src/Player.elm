module Player
    exposing
        ( renderPlayer
        , initialPlayer
        , updatePlayer
        )

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import Dict exposing (Dict)
import Coordinates exposing (locationToGridCoordinate)
import GamePlatform exposing (Platform, platformSize)
import CollisionHelpers
    exposing
        ( getGridCoordinatesPlayerIsOverlapping
        , getCollisionWithDisplacement
        , CollisionDirection(CollisionNegativeDirection, CollisionPositiveDirection)
        )
import GameTypes
    exposing
        ( Player
        , Vector
        , vectorIntToFloat
        , IntVector
        , PlayerState(OnTheGround, Dashing, RecoveringFromDash, SlowingToStop, InTheAir)
        , TempProperties
        )
import Controller
    exposing
        ( Controller
        , ButtonState
            ( Pressed
            , Released
            )
        , DPadHorizontal(DPadRight, DPadLeft, NoHorizontalDPad)
        , isButtonDown
        )


initialPlayer : Player
initialPlayer =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , playerState = InTheAir 0
    }



-------------------------------
-- constants
-------------------------------


playerSpriteSize : IntVector
playerSpriteSize =
    ( 128, 128 )


playerHitBoxSize : IntVector
playerHitBoxSize =
    ( 64, 64 )



--- Forces


noForce : Float
noForce =
    0



--- Friction


baseFrictionCoefficent : Float
baseFrictionCoefficent =
    0.1


dragCoefficentZero : Float
dragCoefficentZero =
    0


lightDragCoefficent : Float
lightDragCoefficent =
    0.001


mediumDragCoefficent : Float
mediumDragCoefficent =
    0.002


heavyDragCoefficent : Float
heavyDragCoefficent =
    0.003


maxDragCoefficent : Float
maxDragCoefficent =
    0.004


fullStop : Float
fullStop =
    0



-------------------------------
-- player helper functions
-------------------------------


getPlayerLeftKickPoint : Vector -> Vector
getPlayerLeftKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            getX playerSpriteSize - getX playerHitBoxSize

        spriteBoxLeftSide =
            x - toFloat spriteHitBoxSizeDif / 2

        kickPointY =
            ((toFloat <| getY playerHitBoxSize) / 2) + y
    in
        ( spriteBoxLeftSide, kickPointY )


getPlayerRightKickPoint : Vector -> Vector
getPlayerRightKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            getX playerSpriteSize - getX playerHitBoxSize

        spriteBoxRightSide =
            toFloat spriteHitBoxSizeDif / 2 + (toFloat <| getX playerHitBoxSize) + x

        kickPointY =
            ((toFloat <| getY playerHitBoxSize) / 2) + y
    in
        ( spriteBoxRightSide, kickPointY )


wallsNearPlayer : Dict IntVector Platform -> Player -> WallsNearPlayer
wallsNearPlayer platforms player =
    let
        wallToTheRight =
            ( player.x, player.y )
                |> getPlayerRightKickPoint
                |> locationToGridCoordinate
                |> flip Dict.member platforms

        wallToTheLeft =
            ( player.x, player.y )
                |> getPlayerLeftKickPoint
                |> locationToGridCoordinate
                |> flip Dict.member platforms
    in
        case ( wallToTheLeft, wallToTheRight ) of
            ( True, True ) ->
                WallOnLeftAndRight

            ( False, False ) ->
                NoWalls

            ( True, False ) ->
                WallOnLeft

            ( False, True ) ->
                WallOnRight


getDirectionFromVelocity : Float -> Direction
getDirectionFromVelocity velocity =
    if velocity < 0 then
        Left
    else
        Right


applyDirectionToForce : Float -> Direction -> Float
applyDirectionToForce force direction =
    case direction of
        Left ->
            -force

        Right ->
            force


addToXVelocity : Player -> Float -> Player
addToXVelocity player acceleration =
    { player
        | vx = player.vx + acceleration
    }


addAccelerationToYVelocity : Player -> Float -> Player
addAccelerationToYVelocity player acceleration =
    { player
        | vy = player.vy + acceleration
    }


getPlayerColor : PlayerState -> Int -> Color.Color
getPlayerColor playerState dashDuration =
    case playerState of
        Dashing frameNumber ->
            Color.yellow

        RecoveringFromDash _ ->
            Color.purple

        SlowingToStop _ ->
            Color.red

        OnTheGround _ ->
            Color.green

        InTheAir _ ->
            Color.blue


pressingInDirectionOfDirection : DPadHorizontal -> Direction -> Bool
pressingInDirectionOfDirection dPadHorizontal direction =
    dPadHorizontal
        == DPadLeft
        && direction
        == Left
        || dPadHorizontal
        == DPadRight
        && direction
        == Right


pressingInDirectionOfVelocity : DPadHorizontal -> Float -> Bool
pressingInDirectionOfVelocity dPadHorizontal playerVelocity =
    dPadHorizontal
        == DPadLeft
        && (getDirectionFromVelocity playerVelocity)
        == Left
        || dPadHorizontal
        == DPadRight
        && (getDirectionFromVelocity playerVelocity)
        == Right


pressingInOppositeDirectionOfVelocity : DPadHorizontal -> Float -> Bool
pressingInOppositeDirectionOfVelocity dPadHorizontal playerVelocity =
    dPadHorizontal
        == DPadLeft
        && (getDirectionFromVelocity playerVelocity)
        == Right
        || dPadHorizontal
        == DPadRight
        && (getDirectionFromVelocity playerVelocity)
        == Left


displacePlayerHorizontally : Player -> Maybe CollisionDirection -> ( Maybe Direction, Player )
displacePlayerHorizontally player collision =
    case collision of
        Nothing ->
            ( Nothing, player )

        Just collision ->
            case collision of
                CollisionNegativeDirection overlap ->
                    ( Just Left
                    , { player
                        | x = player.x + overlap
                        , vx = fullStop
                      }
                    )

                CollisionPositiveDirection overlap ->
                    ( Just Right
                    , { player
                        | x = player.x - overlap
                        , vx = fullStop
                      }
                    )


displacePlayerVerically : Player -> Maybe CollisionDirection -> Player
displacePlayerVerically player collision =
    case collision of
        Nothing ->
            player

        Just collision ->
            case collision of
                CollisionNegativeDirection overlap ->
                    { player
                        | y = player.y + overlap
                        , vy = fullStop
                        , playerState = playerStateAfterCollisionWithGround player.playerState
                    }

                CollisionPositiveDirection overlap ->
                    { player
                        | y = player.y - overlap
                        , vy = fullStop
                    }


playerStateAfterCollisionWithGround : PlayerState -> PlayerState
playerStateAfterCollisionWithGround currentState =
    case currentState of
        Dashing frameNumber ->
            Dashing frameNumber

        RecoveringFromDash frameNumber ->
            RecoveringFromDash frameNumber

        SlowingToStop frameNumber ->
            SlowingToStop frameNumber

        OnTheGround frameNumber ->
            OnTheGround frameNumber

        InTheAir frameNumber ->
            OnTheGround 0



-------------------------------
-- forces helper functions
-------------------------------


calculateYGravityFromJumpProperties : Float -> Int -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    negate ((2 * maxJumpHeight) / toFloat (framesToApex * framesToApex))


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs <| (initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight))


calculateDragCoefficent : Float -> Float -> Float
calculateDragCoefficent acceleration maxSpeed =
    -- yuck!
    abs ((acceleration - baseFrictionCoefficent) / ((maxSpeed + acceleration) * (maxSpeed + acceleration)))
        |> Debug.log "drag: "


getDPadForce : PlayerState -> DPadHorizontal -> Float -> Float
getDPadForce playerState dPadHorizontal dPadAcceleration =
    case playerState of
        RecoveringFromDash _ ->
            noForce

        SlowingToStop _ ->
            noForce

        _ ->
            case dPadHorizontal of
                DPadRight ->
                    dPadAcceleration

                DPadLeft ->
                    -dPadAcceleration

                NoHorizontalDPad ->
                    noForce


handleHorizontalFriction : TempProperties -> Controller -> Float -> Player -> Player
handleHorizontalFriction tempProperties controller velocity player =
    getHorizontalFrictionStrength tempProperties controller.dPadHorizontal controller.dashButton player.vx player.playerState velocity
        |> calculateTotalFriction player.vx
        |> calculateForceDirection player.vx
        |> applyFrictionToVelocity player.vx
        |> (\newVelocity -> { player | vx = newVelocity })


calculateTotalFriction : Float -> Float -> Float
calculateTotalFriction velocity frictionCoefficent =
    baseFrictionCoefficent + ((abs velocity * abs velocity) * frictionCoefficent)


applyFrictionToVelocity : Float -> Float -> Float
applyFrictionToVelocity velocity friction =
    case isFrictionStrongerThanVelocity velocity friction of
        True ->
            fullStop

        False ->
            velocity + friction


isFrictionStrongerThanVelocity : Float -> Float -> Bool
isFrictionStrongerThanVelocity velocity friction =
    abs velocity <= abs friction


calculateForceDirection : Float -> Float -> Float
calculateForceDirection velocity force =
    getDirectionFromVelocity velocity
        |> flipDirection
        |> applyDirectionToForce force


flipDirection : Direction -> Direction
flipDirection direction =
    case direction of
        Right ->
            Left

        Left ->
            Right


handleVerticalFriction : Float -> DPadHorizontal -> Maybe Direction -> Player -> Player
handleVerticalFriction wallFriction horizontalDPad collisionDirection player =
    getVerticalFrictionStrength wallFriction horizontalDPad collisionDirection
        |> calculateTotalFriction player.vy
        |> calculateForceDirection player.vy
        |> applyFrictionToVelocity player.vy
        |> (\newVelocity -> { player | vy = newVelocity })


getHorizontalFrictionStrength : TempProperties -> DPadHorizontal -> ButtonState -> Float -> PlayerState -> Float -> Float
getHorizontalFrictionStrength tempProperties horizontalDPadButton dashButton velocityAfterAcc playerState velocity =
    case playerState of
        RecoveringFromDash _ ->
            dragCoefficentZero

        SlowingToStop _ ->
            maxDragCoefficent

        _ ->
            if pressingInDirectionOfVelocity horizontalDPadButton velocityAfterAcc && isButtonDown dashButton then
                calculateDragCoefficent
                    (abs (velocityAfterAcc - velocity))
                    tempProperties.maxRunningSpeed
            else if pressingInDirectionOfVelocity horizontalDPadButton velocityAfterAcc then
                calculateDragCoefficent
                    (abs (velocityAfterAcc - velocity))
                    tempProperties.maxWalkingSpeed
            else if pressingInOppositeDirectionOfVelocity horizontalDPadButton velocityAfterAcc then
                maxDragCoefficent
            else
                heavyDragCoefficent


getVerticalFrictionStrength : Float -> DPadHorizontal -> Maybe Direction -> Float
getVerticalFrictionStrength wallSlideFrictionCoefficent dPadHorizontal collisionDirection =
    case collisionDirection of
        Nothing ->
            lightDragCoefficent

        Just direction ->
            if pressingInDirectionOfDirection dPadHorizontal direction then
                wallSlideFrictionCoefficent
            else
                lightDragCoefficent



-------------------------------
-- update
-------------------------------


type WallsNearPlayer
    = WallOnLeftAndRight
    | NoWalls
    | WallOnLeft
    | WallOnRight


type Direction
    = Left
    | Right


type PlayerAction
    = Jump
    | WallJump Direction
    | EndJump
    | StartDash Direction
    | StartDashRecover
    | NoAction


type PlayerStateMsg
    = IncrementFrame
    | EndState


updatePlayer : Controller -> TempProperties -> Dict IntVector Platform -> Player -> Player
updatePlayer controller tempProperties platforms player =
    playerStateRoutine tempProperties player
        |> activeUpdate controller tempProperties platforms
        |> runRoutineX tempProperties controller
        |> handleCollisionX platforms
        |> runRoutineY tempProperties controller
        |> handleCollisionY platforms


playerStateRoutine : TempProperties -> Player -> Player
playerStateRoutine tempProperties player =
    calculatePlayerStateRoutineAction tempProperties player.playerState player.vx
        |> runPlayerStateRoutine player.playerState
        |> setPlayerState player


calculatePlayerStateRoutineAction : TempProperties -> PlayerState -> Float -> PlayerStateMsg
calculatePlayerStateRoutineAction tempProperties playerState playerXVelocity =
    case playerState of
        Dashing frameNumber ->
            if frameNumber > tempProperties.dashDuration then
                EndState
            else
                IncrementFrame

        RecoveringFromDash frameNumber ->
            if frameNumber > tempProperties.dashRecoveryDuration then
                EndState
            else
                IncrementFrame

        SlowingToStop frameNumber ->
            if playerXVelocity == 0 then
                EndState
            else
                IncrementFrame

        OnTheGround _ ->
            IncrementFrame

        InTheAir _ ->
            IncrementFrame


runPlayerStateRoutine : PlayerState -> PlayerStateMsg -> PlayerState
runPlayerStateRoutine playerState playerStateMsg =
    case playerStateMsg of
        IncrementFrame ->
            incrementFrameNumber playerState

        EndState ->
            case playerState of
                Dashing _ ->
                    SlowingToStop 0

                _ ->
                    InTheAir 0


incrementFrameNumber : PlayerState -> PlayerState
incrementFrameNumber playerState =
    case playerState of
        Dashing frameNumber ->
            Dashing (frameNumber + 1)

        RecoveringFromDash frameNumber ->
            RecoveringFromDash (frameNumber + 1)

        SlowingToStop frameNumber ->
            SlowingToStop (frameNumber + 1)

        OnTheGround frameNumber ->
            OnTheGround (frameNumber + 1)

        InTheAir frameNumber ->
            InTheAir (frameNumber + 1)


setPlayerState : Player -> PlayerState -> Player
setPlayerState player playerState =
    { player
        | playerState = playerState
    }


activeUpdate : Controller -> TempProperties -> Dict IntVector Platform -> Player -> Player
activeUpdate controller tempProperties platforms player =
    calculatePlayerAction tempProperties controller platforms player
        |> actionUpdate tempProperties player


inDashRecoverWindow : Int -> Int -> Int -> Bool
inDashRecoverWindow frameNumber dashDuration buttonPressWindow =
    -- primitives suck!
    frameNumber > dashDuration - buttonPressWindow && frameNumber < dashDuration


calculatePlayerAction : TempProperties -> Controller -> Dict IntVector Platform -> Player -> PlayerAction
calculatePlayerAction tempProperties controller platforms player =
    -- don't need to pass in the whole player and only needs the jump and dash button states
    case player.playerState of
        Dashing frameNumber ->
            if controller.jumpButton == Pressed then
                Jump
            else if controller.dashButton == Pressed && inDashRecoverWindow frameNumber tempProperties.dashDuration tempProperties.buttonPressWindow then
                StartDashRecover
            else
                NoAction

        RecoveringFromDash frameNumber ->
            if controller.dashButton == Pressed && frameNumber > tempProperties.dashRecoveryDuration - tempProperties.buttonPressWindow then
                StartDash <| getDirectionFromVelocity player.vx
            else
                NoAction

        SlowingToStop _ ->
            NoAction

        OnTheGround _ ->
            if controller.jumpButton == Pressed then
                Jump
            else if controller.dashButton == Pressed then
                StartDash <| getDirectionFromVelocity player.vx
            else
                NoAction

        InTheAir _ ->
            if controller.jumpButton == Pressed then
                case wallsNearPlayer platforms player of
                    NoWalls ->
                        NoAction

                    WallOnLeftAndRight ->
                        NoAction

                    WallOnLeft ->
                        WallJump Right

                    WallOnRight ->
                        WallJump Left
            else if controller.jumpButton == Released then
                EndJump
            else
                NoAction


actionUpdate : TempProperties -> Player -> PlayerAction -> Player
actionUpdate tempProperties player action =
    case action of
        Jump ->
            let
                jumpVelocityY =
                    calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                        |> calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight
            in
                { player
                    | playerState = InTheAir 0
                    , vy = jumpVelocityY
                }

        WallJump direction ->
            let
                jumpVelocityY =
                    calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                        |> calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight

                jumpVelocityX =
                    case direction of
                        Left ->
                            -jumpVelocityY

                        Right ->
                            jumpVelocityY
            in
                { player
                    | vx = jumpVelocityX
                    , vy = jumpVelocityY
                }

        EndJump ->
            let
                baseGravity =
                    calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex

                baseJumpVelocityY =
                    calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight baseGravity

                earlyJumpTerminationVelocity =
                    calculateEarlyJumpTerminationVelocity baseJumpVelocityY baseGravity tempProperties.maxJumpHeight tempProperties.minJumpHeight
            in
                { player | vy = min earlyJumpTerminationVelocity player.vy }

        StartDash dashDirection ->
            let
                initialDashingVelocity =
                    case dashDirection of
                        Left ->
                            -tempProperties.maxRunningSpeed

                        Right ->
                            tempProperties.maxRunningSpeed
            in
                { player
                    | playerState = Dashing 0
                    , vx = initialDashingVelocity
                }

        StartDashRecover ->
            { player | playerState = RecoveringFromDash 0 }

        NoAction ->
            player


runRoutineX : TempProperties -> Controller -> Player -> Player
runRoutineX tempProperties controller player =
    getDPadForce player.playerState controller.dPadHorizontal tempProperties.dPadAcceleration
        |> addToXVelocity player
        |> handleHorizontalFriction tempProperties controller player.vx
        |> (\player -> { player | x = player.x + player.vx })


runRoutineY : TempProperties -> Controller -> ( Maybe Direction, Player ) -> Player
runRoutineY tempProperties controller ( collision, player ) =
    -- maxWallSlideSpeed
    calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
        |> addAccelerationToYVelocity player
        |> handleVerticalFriction 1 controller.dPadHorizontal collision
        |> (\player -> { player | y = player.y + player.vy })


handleCollisionX : Dict IntVector Platform -> Player -> ( Maybe Direction, Player )
handleCollisionX platforms player =
    getGridCoordinatesPlayerIsOverlapping player.x player.y playerHitBoxSize platforms
        |> List.filter (\coord -> Dict.member coord platforms)
        |> List.map (\( x, _ ) -> getCollisionWithDisplacement (getX playerHitBoxSize) player.x (getX platformSize) (toFloat x))
        |> List.head
        |> displacePlayerHorizontally player


handleCollisionY : Dict IntVector Platform -> Player -> Player
handleCollisionY platforms player =
    getGridCoordinatesPlayerIsOverlapping player.x player.y playerHitBoxSize platforms
        |> List.filter (\coord -> Dict.member coord platforms)
        |> List.map (\( _, y ) -> getCollisionWithDisplacement (getY playerHitBoxSize) player.y (getY platformSize) (toFloat y))
        |> List.head
        |> displacePlayerVerically player



-------------------------------
-- render
-------------------------------


renderPlayer : Resources -> Player -> Int -> List Renderable
renderPlayer resources player dashDuration =
    let
        { x, y, playerState } =
            player

        playerColor =
            getPlayerColor player.playerState dashDuration

        hitBox =
            Render.shape
                Render.rectangle
                { color = playerColor
                , position = ( x, y )
                , size = vectorIntToFloat playerHitBoxSize
                }

        fullSprite =
            Render.sprite
                { texture = Resources.getTexture "./assets/player-background-glow.png" resources
                , position = ( x - 32, y - 32 )
                , size = vectorIntToFloat playerSpriteSize
                }
    in
        [ fullSprite
        , hitBox
        ]
