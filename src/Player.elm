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
        , PlayerState(OnTheGround, Dashing, RecoveringFromDash, InTheAir)
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



--- Friction


noFriction : Float
noFriction =
    1


lightFriction : Float
lightFriction =
    0.98


mediumFriction : Float
mediumFriction =
    0.97


heavyFriction : Float
heavyFriction =
    0.95


maxFriction : Float
maxFriction =
    0.93


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


addAccelerationToXVelocity : Player -> Float -> Player
addAccelerationToXVelocity player acceleration =
    { player
        | vx = player.vx + acceleration
    }


addAccelerationToYVelocity : Float -> Player -> Player
addAccelerationToYVelocity acceleration player =
    { player
        | vy = player.vy + acceleration
    }


getPlayerColor : PlayerState -> Color.Color
getPlayerColor playerState =
    case playerState of
        Dashing _ ->
            Color.yellow

        RecoveringFromDash _ ->
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

        OnTheGround frameNumber ->
            OnTheGround frameNumber

        InTheAir frameNumber ->
            OnTheGround 0



-------------------------------
-- forces helper functions
-------------------------------


calculateYGravityFromJumpProperties : Float -> Int -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    (2 * maxJumpHeight) / toFloat (framesToApex * framesToApex)


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs ((initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight)))


getDPadForce : DPadHorizontal -> Float -> Float
getDPadForce dPadHorizontal dPadAcceleration =
    case dPadHorizontal of
        DPadRight ->
            dPadAcceleration

        DPadLeft ->
            -dPadAcceleration

        NoHorizontalDPad ->
            0


applyHorizontalFriction : Float -> Player -> Player
applyHorizontalFriction friction player =
    { player | vx = player.vx * friction }


applyVerticalFriction : Float -> Player -> Player
applyVerticalFriction friction player =
    { player | vy = player.vy * friction }


calculateHorizontalFriction : DPadHorizontal -> ButtonState -> PlayerState -> Float -> Float
calculateHorizontalFriction dPadHorizontal dashButton playerState xVelocity =
    case playerState of
        RecoveringFromDash _ ->
            noFriction

        _ ->
            if pressingInDirectionOfVelocity dPadHorizontal xVelocity && isButtonDown dashButton then
                lightFriction
            else if pressingInDirectionOfVelocity dPadHorizontal xVelocity then
                mediumFriction
            else if pressingInOppositeDirectionOfVelocity dPadHorizontal xVelocity then
                maxFriction
            else
                heavyFriction


calculateVerticalFriction : Float -> DPadHorizontal -> Maybe Direction -> Float
calculateVerticalFriction wallSlideFriction dPadHorizontal collisionDirection =
    case collisionDirection of
        Nothing ->
            noFriction

        Just direction ->
            if pressingInDirectionOfDirection dPadHorizontal direction then
                wallSlideFriction
            else
                noFriction



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
    calculatePlayerStateRoutineAction tempProperties player.playerState
        |> runPlayerStateRoutine player.playerState
        |> setPlayerState player


calculatePlayerStateRoutineAction : TempProperties -> PlayerState -> PlayerStateMsg
calculatePlayerStateRoutineAction tempProperties playerState =
    case playerState of
        Dashing frameNumber ->
            if frameNumber > tempProperties.dashDuration + tempProperties.buttonPressWindow then
                EndState
            else
                IncrementFrame

        RecoveringFromDash frameNumber ->
            if frameNumber > tempProperties.dashRecoveryDuration + tempProperties.buttonPressWindow then
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
            InTheAir 0


incrementFrameNumber : PlayerState -> PlayerState
incrementFrameNumber playerState =
    case playerState of
        Dashing frameNumber ->
            Dashing (frameNumber + 1)

        RecoveringFromDash frameNumber ->
            RecoveringFromDash (frameNumber + 1)

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


calculatePlayerAction : TempProperties -> Controller -> Dict IntVector Platform -> Player -> PlayerAction
calculatePlayerAction tempProperties controller platforms player =
    -- don't need to pass in the whole player and only needs the jump and dash button states
    case player.playerState of
        Dashing frameNumber ->
            if controller.jumpButton == Pressed then
                Jump
            else if controller.dashButton == Pressed && frameNumber > tempProperties.dashDuration then
                StartDashRecover
            else
                NoAction

        RecoveringFromDash frameNumber ->
            if controller.dashButton == Pressed && frameNumber > tempProperties.dashRecoveryDuration then
                StartDash <| getDirectionFromVelocity player.vx
            else
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
                        |> negate
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
                        |> negate
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
                        |> negate

                baseJumpVelocityY =
                    calculateInitialJumpVelocityFromJumpProperties tempProperties.maxJumpHeight baseGravity

                earlyJumpTerminationVelocity =
                    calculateEarlyJumpTerminationVelocity baseJumpVelocityY baseGravity tempProperties.maxJumpHeight tempProperties.minJumpHeight
            in
                { player | vy = earlyJumpTerminationVelocity }

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
    -- don't need the whole controller
    let
        friction =
            calculateHorizontalFriction controller.dPadHorizontal controller.dashButton player.playerState player.vx
    in
        getDPadForce controller.dPadHorizontal tempProperties.dPadAcceleration
            |> addAccelerationToXVelocity player
            |> applyHorizontalFriction friction
            |> (\player -> { player | x = player.x + player.vx })


runRoutineY : TempProperties -> Controller -> ( Maybe Direction, Player ) -> Player
runRoutineY tempProperties controller ( collision, player ) =
    -- don't need the whole controller
    let
        gravity =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
                |> negate

        friction =
            calculateVerticalFriction tempProperties.wallFriction controller.dPadHorizontal collision
    in
        addAccelerationToYVelocity gravity player
            |> applyVerticalFriction friction
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


renderPlayer : Resources -> Player -> ( Int, Int ) -> List Renderable
renderPlayer resources player ( dash, recover ) =
    let
        { x, y, playerState } =
            player

        playerColor =
            getPlayerColor player.playerState

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
