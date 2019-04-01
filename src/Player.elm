module Player exposing
    ( initialPlayer
    , renderPlayer
    , updatePlayer
    )

import CollisionHelpers
    exposing
        ( CollisionDirection(..)
        , getCollisionWithDisplacement
        , getGridCoordinatesPlayerIsOverlapping
        )
import Color
import Controller
    exposing
        ( ButtonState(..)
        , Controller
        , DPadHorizontal(..)
        , isButtonDown
        )
import Coordinates exposing (locationToGridCoordinate)
import Dict exposing (Dict)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import GamePlatform
import GameTypes
    exposing
        ( IntVector
        , Player
        , PlayerState(..)
        , TempProperties
        , Vector
        , vectorIntToFloat
        )
import V2


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


getBottomCenterSpritePoint : Vector -> Vector
getBottomCenterSpritePoint ( x, y ) =
    let
        halfHitBoxWidth =
            (toFloat <| Tuple.first playerHitBoxSize) / 2

        center =
            x + halfHitBoxWidth

        spriteHitBoxSizeDif =
            Tuple.second playerSpriteSize - Tuple.second playerHitBoxSize

        spriteBoxBottomSide =
            y - (toFloat spriteHitBoxSizeDif / 2)
    in
    ( center, spriteBoxBottomSide )


getPlayerLeftKickPoint : Vector -> Vector
getPlayerLeftKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            Tuple.first playerSpriteSize - Tuple.first playerHitBoxSize

        spriteBoxLeftSide =
            x - toFloat spriteHitBoxSizeDif / 2

        kickPointY =
            ((toFloat <| Tuple.second playerHitBoxSize) / 2) + y
    in
    ( spriteBoxLeftSide, kickPointY )


getPlayerRightKickPoint : Vector -> Vector
getPlayerRightKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            Tuple.first playerSpriteSize - Tuple.first playerHitBoxSize

        spriteBoxRightSide =
            toFloat spriteHitBoxSizeDif / 2 + (toFloat <| Tuple.first playerHitBoxSize) + x

        kickPointY =
            ((toFloat <| Tuple.second playerHitBoxSize) / 2) + y
    in
    ( spriteBoxRightSide, kickPointY )


wallsNearPlayer : Dict IntVector GamePlatform.Platform -> Float -> Float -> WallsNearPlayer
wallsNearPlayer platforms playerX playerY =
    let
        wallToTheRight =
            ( playerX, playerY )
                |> getPlayerRightKickPoint
                |> locationToGridCoordinate
                |> V2.flip Dict.member platforms

        wallToTheLeft =
            ( playerX, playerY )
                |> getPlayerLeftKickPoint
                |> locationToGridCoordinate
                |> V2.flip Dict.member platforms
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


groundBelowPlayer : Dict IntVector GamePlatform.Platform -> Float -> Float -> Bool
groundBelowPlayer platforms playerX playerY =
    ( playerX, playerY )
        |> getBottomCenterSpritePoint
        |> locationToGridCoordinate
        |> V2.flip Dict.member platforms


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


getPlayerColor : PlayerState -> Color.Color
getPlayerColor playerState =
    case playerState of
        OnTheGround _ ->
            Color.green

        GroundDash _ ->
            Color.yellow

        InTheAir _ ->
            Color.blue

        AirDash _ ->
            Color.orange

        Falling _ ->
            Color.red


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
        && getDirectionFromVelocity playerVelocity
        == Left
        || dPadHorizontal
        == DPadRight
        && getDirectionFromVelocity playerVelocity
        == Right


pressingInOppositeDirectionOfVelocity : DPadHorizontal -> Float -> Bool
pressingInOppositeDirectionOfVelocity dPadHorizontal playerVelocity =
    dPadHorizontal
        == DPadLeft
        && getDirectionFromVelocity playerVelocity
        == Right
        || dPadHorizontal
        == DPadRight
        && getDirectionFromVelocity playerVelocity
        == Left


displacePlayerHorizontally : Player -> Maybe CollisionDirection -> ( Maybe Direction, Player )
displacePlayerHorizontally player maybeCollision =
    case maybeCollision of
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
displacePlayerVerically player maybeCollision =
    case maybeCollision of
        Nothing ->
            { player
                | playerState = playerStateAfterNoCollisionWithGround player.playerState
            }

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
playerStateAfterCollisionWithGround playerState =
    case playerState of
        OnTheGround frameNumber ->
            OnTheGround frameNumber

        GroundDash frameNumber ->
            GroundDash frameNumber

        InTheAir _ ->
            OnTheGround 0

        AirDash _ ->
            OnTheGround 0

        Falling _ ->
            OnTheGround 0


playerStateAfterNoCollisionWithGround : PlayerState -> PlayerState
playerStateAfterNoCollisionWithGround playerState =
    case playerState of
        OnTheGround _ ->
            InTheAir 0

        GroundDash frameNumber ->
            GroundDash frameNumber

        InTheAir frameNumber ->
            InTheAir frameNumber

        AirDash frameNumber ->
            AirDash frameNumber

        Falling frameNumber ->
            Falling frameNumber



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


calculateWallSlideFriction : Float -> Float -> Float
calculateWallSlideFriction gravitationalAcceleration maxSpeed =
    (negate <| abs gravitationalAcceleration) / (maxSpeed * maxSpeed)


calculateDragCoefficent : Float -> Float -> Float
calculateDragCoefficent acceleration maxSpeed =
    -- yuck!
    abs ((acceleration - baseFrictionCoefficent) / ((maxSpeed + acceleration) * (maxSpeed + acceleration)))


getDPadForce : DPadHorizontal -> Float -> Float
getDPadForce dPadHorizontal dPadAcceleration =
    case dPadHorizontal of
        DPadRight ->
            dPadAcceleration

        DPadLeft ->
            -dPadAcceleration

        NoHorizontalDPad ->
            noForce


handleHorizontalFriction : TempProperties -> Controller -> Float -> Player -> Player
handleHorizontalFriction tempProperties controller velocity player =
    getHorizontalFrictionStrength tempProperties controller.dPadHorizontal controller.dashButton player.vx velocity
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


handleVerticalFriction : TempProperties -> DPadHorizontal -> Maybe Direction -> Float -> Player -> Player
handleVerticalFriction tempProperties horizontalDPad collisionDirection acceleration player =
    getVerticalFrictionStrength tempProperties horizontalDPad collisionDirection acceleration
        |> calculateTotalFriction player.vy
        |> calculateForceDirection player.vy
        |> applyFrictionToVelocity player.vy
        |> (\newVelocity -> { player | vy = newVelocity })


getHorizontalFrictionStrength : TempProperties -> DPadHorizontal -> ButtonState -> Float -> Float -> Float
getHorizontalFrictionStrength tempProperties horizontalDPadButton dashButton velocityAfterAcc velocity =
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


getVerticalFrictionStrength : TempProperties -> DPadHorizontal -> Maybe Direction -> Float -> Float
getVerticalFrictionStrength tempProperties dPadHorizontal collisionDirection acceleration =
    case collisionDirection of
        Nothing ->
            lightDragCoefficent

        Just direction ->
            if pressingInDirectionOfDirection dPadHorizontal direction then
                calculateDragCoefficent (abs acceleration) tempProperties.maxWallSlideSpeed
                -- |> Debug.log "vertical drag coefficent: "

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
    | StartDash DashType
    | NoAction


type DashType
    = Ground
    | Air


type PlayerStateMsg
    = IncrementFrame
    | EndState


updatePlayer : Controller -> TempProperties -> Dict IntVector GamePlatform.Platform -> Player -> Player
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
        GroundDash frameNumber ->
            if frameNumber > tempProperties.dashDuration then
                EndState

            else
                IncrementFrame

        AirDash frameNumber ->
            if frameNumber > tempProperties.dashDuration then
                EndState

            else
                IncrementFrame

        InTheAir _ ->
            IncrementFrame

        Falling _ ->
            IncrementFrame

        OnTheGround _ ->
            IncrementFrame


runPlayerStateRoutine : PlayerState -> PlayerStateMsg -> PlayerState
runPlayerStateRoutine playerState playerStateMsg =
    case playerStateMsg of
        IncrementFrame ->
            incrementFrameNumber playerState

        EndState ->
            case playerState of
                GroundDash _ ->
                    InTheAir 0

                AirDash _ ->
                    Falling 0

                _ ->
                    playerState


incrementFrameNumber : PlayerState -> PlayerState
incrementFrameNumber playerState =
    case playerState of
        GroundDash frameNumber ->
            GroundDash (frameNumber + 1)

        AirDash frameNumber ->
            AirDash (frameNumber + 1)

        Falling frameNumber ->
            Falling (frameNumber + 1)

        OnTheGround frameNumber ->
            OnTheGround (frameNumber + 1)

        InTheAir frameNumber ->
            InTheAir (frameNumber + 1)


setPlayerState : Player -> PlayerState -> Player
setPlayerState player playerState =
    { player
        | playerState = playerState
    }


activeUpdate : Controller -> TempProperties -> Dict IntVector GamePlatform.Platform -> Player -> Player
activeUpdate controller tempProperties platforms player =
    calculatePlayerAction tempProperties controller platforms player
        |> actionUpdate tempProperties player


canDashAgain : Int -> Int -> Int -> Bool
canDashAgain frameNumber dashDuration buttonPressWindow =
    -- primitives suck!!!!!
    frameNumber > dashDuration - buttonPressWindow


determineDashType : Dict IntVector GamePlatform.Platform -> Float -> Float -> DashType
determineDashType platforms playerX playerY =
    case groundBelowPlayer platforms playerX playerY of
        True ->
            Ground

        False ->
            Air


calculatePlayerAction : TempProperties -> Controller -> Dict IntVector GamePlatform.Platform -> Player -> PlayerAction
calculatePlayerAction tempProperties controller platforms player =
    -- don't need to pass in the whole player and only needs the jump and dash button states
    case player.playerState of
        GroundDash frameNumber ->
            if controller.jumpButton == Pressed then
                Jump

            else if controller.dashButton == Pressed && canDashAgain frameNumber tempProperties.dashDuration tempProperties.buttonPressWindow then
                StartDash <| determineDashType platforms player.x player.y

            else
                NoAction

        AirDash _ ->
            NoAction

        OnTheGround _ ->
            if controller.jumpButton == Pressed then
                Jump

            else if controller.dashButton == Pressed then
                StartDash Ground

            else
                NoAction

        InTheAir _ ->
            if controller.jumpButton == Pressed then
                case wallsNearPlayer platforms player.x player.y of
                    NoWalls ->
                        NoAction

                    WallOnLeftAndRight ->
                        NoAction

                    WallOnLeft ->
                        WallJump Right

                    WallOnRight ->
                        WallJump Left

            else if controller.dashButton == Pressed then
                StartDash Air

            else if controller.jumpButton == Released then
                EndJump

            else
                NoAction

        Falling _ ->
            if controller.jumpButton == Pressed then
                case wallsNearPlayer platforms player.x player.y of
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
                            -jumpVelocityY * 0.7

                        Right ->
                            jumpVelocityY * 0.7
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

        StartDash dashType ->
            let
                speedAfterDash =
                    (abs player.vx + 10)
                        |> clamp 0 tempProperties.maxDashingSpeed

                dashVelocity =
                    max speedAfterDash <| abs player.vx

                finalDashVelocity =
                    getDirectionFromVelocity player.vx
                        |> applyDirectionToForce dashVelocity

                dash =
                    case dashType of
                        Ground ->
                            GroundDash 0

                        Air ->
                            AirDash 0
            in
            { player
                | playerState = dash
                , vx = finalDashVelocity
            }

        NoAction ->
            player


runRoutineX : TempProperties -> Controller -> Player -> Player
runRoutineX tempProperties controller thePlayer =
    getDPadForce controller.dPadHorizontal tempProperties.dPadAcceleration
        |> addToXVelocity thePlayer
        |> handleHorizontalFriction tempProperties controller thePlayer.vx
        |> (\player -> { player | x = player.x + player.vx })


runRoutineY : TempProperties -> Controller -> ( Maybe Direction, Player ) -> Player
runRoutineY tempProperties controller ( collision, thePlayer ) =
    -- maxWallSlideSpeed
    let
        gravity =
            calculateYGravityFromJumpProperties tempProperties.maxJumpHeight tempProperties.framesToApex
    in
    gravity
        |> addAccelerationToYVelocity thePlayer
        |> handleVerticalFriction tempProperties controller.dPadHorizontal collision gravity
        |> (\player -> { player | y = player.y + player.vy })


handleCollisionX : Dict IntVector GamePlatform.Platform -> Player -> ( Maybe Direction, Player )
handleCollisionX platforms player =
    getGridCoordinatesPlayerIsOverlapping player.x player.y playerHitBoxSize platforms
        |> List.filter (\coord -> Dict.member coord platforms)
        |> List.map (\( x, _ ) -> getCollisionWithDisplacement (Tuple.first playerHitBoxSize) player.x (Tuple.first Coordinates.gridSquareSize) (toFloat x))
        |> List.head
        |> displacePlayerHorizontally player


handleCollisionY : Dict IntVector GamePlatform.Platform -> Player -> Player
handleCollisionY platforms player =
    getGridCoordinatesPlayerIsOverlapping player.x player.y playerHitBoxSize platforms
        |> List.filter (\coord -> Dict.member coord platforms)
        |> List.map (\( _, y ) -> getCollisionWithDisplacement (Tuple.second playerHitBoxSize) player.y (Tuple.second Coordinates.gridSquareSize) (toFloat y))
        |> List.head
        |> displacePlayerVerically player



-------------------------------
-- render
-------------------------------


renderPlayer : Resources -> Player -> Dict IntVector GamePlatform.Platform -> List Renderable
renderPlayer resources player platforms =
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

        -- ( currentFrame, totalFrames, spriteSheet ) =
        --     determineSpriteFrame player
        --
        -- direction =
        --     getDirectionFromVelocity player.vx
        --         |> applyDirectionToForce 1
        --
        -- playerSrite =
        --     Render.manuallyManagedAnimatedSpriteWithOptions
        --         { bottomLeft = ( 0, 0 )
        --         , topRight = ( 1, 1 )
        --         , currentFrame = currentFrame
        --         , numberOfFrames = totalFrames
        --         , pivot = ( 0.5, 0.5 )
        --         , position = ( x + 32, y + 48, 0 )
        --         , rotation = 0
        --         , size = (\( x, y ) -> ( direction * x, y )) <| vectorIntToFloat playerSpriteSize
        --         , texture = Resources.getTexture ("./assets/" ++ spriteSheet) resources
        --         }
    in
    [ fullSprite
    , hitBox

    -- , playerSrite
    ]



-- determineSpriteFrame : Player -> ( Int, Int, String )
-- determineSpriteFrame player =
--     case player.playerState of
--         OnTheGround frameNumber ->
--             if player.vx == 0 then
--                 ( frameNumber // 20, 2, "idling-blob-sprite-sheet.png" )
--             else
--                 ( frameNumber // 8, 8, "running-blob-sprite-sheet.png" )
--
--         InTheAir _ ->
--             let
--                 spriteFrame =
--                     if player.vy > 20 then
--                         0
--                     else if player.vy > 17 then
--                         1
--                     else if player.vy > 14 then
--                         2
--                     else if player.vy > 5 then
--                         3
--                     else if player.vy > 0 then
--                         4
--                     else
--                         5
--             in
--                 ( spriteFrame, 8, "jumping-blob-sprite-sheet.png" )
--
--         Dashing frameNumber ->
--             ( frameNumber // 8, 4, "dashing-blob-sprite-sheet.png" )
--
--         SlowingToStop frameNumber ->
--             let
--                 spriteFrame =
--                     if frameNumber < 5 then
--                         0
--                     else if frameNumber < 10 then
--                         1
--                     else if frameNumber < 15 then
--                         2
--                     else
--                         4
--             in
--                 ( spriteFrame, 8, "tripping-blob-sprite-sheet.png" )
--
--         RecoveringFromDash frameNumber ->
--             ( frameNumber // 10, 4, "recovering-blob-sprite-sheet.png" )
