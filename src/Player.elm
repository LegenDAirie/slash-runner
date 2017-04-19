module Player exposing (Player, PlayerState(..), updatePlayer, renderPlayer)

import Vector2 as V2 exposing (getX, getY)
import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import Collision2D
import GameTypes exposing (Vector)
import Coordinates exposing (centerToBottomLeftLocationConverter)
import Controller exposing (DPad(..), ControllerState, ButtonState(..))
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance)
import Enemy exposing (Enemy)
import Wall exposing (Wall)
import CollisionHelpers exposing (setByPlatform, getSideCollidingWithEnemies)


type alias Player =
    { location : Vector
    , velocity : Vector
    , playerState : PlayerState
    , size : Vector
    , framesSinceLastChain : Int
    }


type PlayerState
    = Running
    | Jumping Vector
    | Falling
    | Dashing ( Int, Vector )
    | DashRecovery ( Int, Bool )
    | OnWall ( Int, Bool )
    | HitStun Int


maxWalkingVelocity : Float
maxWalkingVelocity =
    6


maxRunningVelocity : Float
maxRunningVelocity =
    8


maxHorizontalVelocity : Float
maxHorizontalVelocity =
    15


defaultJumpForce : Vector
defaultJumpForce =
    ( 0, 50 )


dampening : Float
dampening =
    0.9


overExtendedDashDampening : Float
overExtendedDashDampening =
    0.9


speedChainSpeedConstant : Float
speedChainSpeedConstant =
    2


maxChainDuration : Int
maxChainDuration =
    180


framesDashingMaxDuration : Int
framesDashingMaxDuration =
    15


framesDashRecoveryMaxDuration : Int
framesDashRecoveryMaxDuration =
    15


framesOnWallMaxDuration : Int
framesOnWallMaxDuration =
    15


hitStunMaxDuration : Int
hitStunMaxDuration =
    60


updatePlayer : List Enemy -> List Wall -> ControllerState -> Player -> Player
updatePlayer enemies walls controllerState player =
    let
        ( newLocation, newVelocity ) =
            applyPhysics controllerState.dPad controllerState.dash player.playerState player.framesSinceLastChain player.location player.velocity

        ( setPlayerLocation, sideCollidingWithPlatform ) =
            setByPlatform newLocation player.size walls Nothing

        sidecollidingWithEnemy =
            getSideCollidingWithEnemies setPlayerLocation player.size enemies Nothing

        ( newPlayerState, newFramesSinceLastChain ) =
            ( player.playerState, player.framesSinceLastChain )
                |> incrementPlayerCounters
                |> stateAfterPlatformCollision sideCollidingWithPlatform
                |> stateAfterControllerInputs controllerState
                |> stateAfterEnemyCollision sidecollidingWithEnemy
    in
        { player
            | location = setPlayerLocation
            , velocity = newVelocity
            , playerState = newPlayerState
            , framesSinceLastChain = newFramesSinceLastChain
        }


applyPhysics : DPad -> ButtonState -> PlayerState -> Int -> Vector -> Vector -> ( Vector, Vector )
applyPhysics dPad dashButton playerState framesSinceLastChain location velocity =
    let
        gravitationalForce =
            gravity

        controllerDirectionalForce =
            if dPad == Left then
                controllerLeftForce
            else if dPad == Right then
                controllerRightForce
            else
                ( 0, 0 )

        velocityCap =
            if dPad == Right || dPad == Left then
                if dashButton == Held then
                    maxRunningVelocity
                else
                    maxWalkingVelocity
            else
                maxWalkingVelocity

        newVelocity =
            velocity
                |> (\( x, y ) -> ( x * resistance, y ))
                |> V2.add gravitationalForce
                |> V2.add controllerDirectionalForce
                |> capHorizontalVelocity velocityCap
                |> capVerticalVelocity 15

        speedDashingConstant =
            if framesSinceLastChain < maxChainDuration then
                if dPad == Left then
                    ( -speedChainSpeedConstant, 0 )
                else if dPad == Right then
                    ( speedChainSpeedConstant, 0 )
                else
                    ( 0, 0 )
            else
                ( 0, 0 )
    in
        case playerState of
            Running ->
                let
                    newLocation =
                        newVelocity
                            |> V2.add speedDashingConstant
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Jumping jumpForce ->
                let
                    newLocation =
                        newVelocity
                            |> V2.add jumpForce
                            |> V2.add speedDashingConstant
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Falling ->
                let
                    newLocation =
                        newVelocity
                            |> V2.add speedDashingConstant
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Dashing ( framesDashing, direction ) ->
                let
                    newVelocity =
                        direction
                            |> V2.scale maxHorizontalVelocity
                            |> V2.add gravitationalForce
                            |> capVerticalVelocity speedCap

                    _ =
                        Debug.log "Velocity" newVelocity

                    newLocation =
                        newVelocity
                            |> V2.add speedDashingConstant
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            DashRecovery ( framesRecovering, onGround ) ->
                let
                    newerVelocity =
                        if onGround then
                            newVelocity
                                |> (\( x, y ) -> ( x * overExtendedDashDampening, y ))
                        else
                            newVelocity

                    newLocation =
                        newerVelocity
                            |> V2.add speedDashingConstant
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newerVelocity )

            OnWall ( framesOnWall, wallOnRight ) ->
                let
                    newerVelocity =
                        if framesOnWall < framesOnWallMaxDuration then
                            ( 0, 0 )
                        else
                            newVelocity

                    newLocation =
                        newerVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newerVelocity )

            HitStun framesHitStuned ->
                let
                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( location, velocity )


incrementPlayerCounters : ( PlayerState, Int ) -> ( PlayerState, Int )
incrementPlayerCounters ( playerState, framesSinceLastChain ) =
    case playerState of
        Running ->
            ( Running, framesSinceLastChain + 1 )

        Jumping jumpForce ->
            ( Jumping jumpForce, framesSinceLastChain + 1 )

        Falling ->
            ( Falling, framesSinceLastChain + 1 )

        Dashing ( framesDashing, direction ) ->
            if framesDashing < framesDashingMaxDuration then
                ( Dashing ( framesDashing + 1, direction ), framesSinceLastChain + 1 )
            else
                ( DashRecovery ( 0, True ), framesSinceLastChain + 1 )

        DashRecovery ( framesRecovering, onGround ) ->
            if onGround then
                if framesRecovering < framesDashRecoveryMaxDuration then
                    ( DashRecovery ( framesRecovering + 1, True ), maxChainDuration )
                else
                    ( Running, framesSinceLastChain + 1 )
            else
                ( DashRecovery ( framesRecovering + 1, False ), framesSinceLastChain + 1 )

        OnWall ( framesOnWall, wallOnRight ) ->
            if framesOnWall < framesOnWallMaxDuration then
                ( OnWall ( framesOnWall + 1, wallOnRight ), framesSinceLastChain + 1 )
            else
                ( OnWall ( framesOnWall + 1, wallOnRight ), maxChainDuration )

        HitStun framesHitStuned ->
            if framesHitStuned < hitStunMaxDuration then
                ( HitStun (framesHitStuned + 1), framesSinceLastChain + 1 )
            else
                ( Running, framesSinceLastChain + 1 )


stateAfterControllerInputs : ControllerState -> ( PlayerState, Int ) -> ( PlayerState, Int )
stateAfterControllerInputs controllerState ( playerState, framesSinceLastChain ) =
    case playerState of
        HitStun framesHitStuned ->
            ( HitStun framesHitStuned, maxChainDuration )

        Falling ->
            ( Falling, framesSinceLastChain )

        Jumping jumpForce ->
            case controllerState.jump of
                Pressed ->
                    ( Jumping jumpForce, framesSinceLastChain )

                Held ->
                    ( Jumping ( getX jumpForce, getY jumpForce * dampening ), framesSinceLastChain )

                Released ->
                    ( Jumping ( 0, 0 ), framesSinceLastChain )

                Inactive ->
                    ( Jumping ( 0, 0 ), framesSinceLastChain )

        Running ->
            let
                newState =
                    if controllerState.dash == Pressed then
                        if controllerState.dPad == Right then
                            Dashing ( 0, ( 1, 0 ) )
                        else if controllerState.dPad == Left then
                            Dashing ( 0, ( -1, 0 ) )
                        else
                            Running
                    else
                        Running

                newerState =
                    if controllerState.jump == Pressed then
                        -- jumping cancels dashing
                        Jumping defaultJumpForce
                    else
                        newState
            in
                ( newerState, framesSinceLastChain )

        Dashing framesDashing ->
            let
                newerState =
                    if controllerState.jump == Pressed then
                        -- jumping cancels dashing
                        Jumping defaultJumpForce
                    else
                        Dashing framesDashing
            in
                ( newerState, framesSinceLastChain )

        DashRecovery ( framesRecovering, onGround ) ->
            if onGround then
                ( DashRecovery ( framesRecovering, True ), maxChainDuration )
            else if controllerState.jump == Pressed then
                ( Jumping defaultJumpForce, framesSinceLastChain )
            else
                ( DashRecovery ( framesRecovering, False ), framesSinceLastChain )

        OnWall ( framesOnWall, wallOnRight ) ->
            let
                kickOffHorizontalVelocity =
                    if controllerState.dash == Held then
                        maxRunningVelocity
                    else
                        maxWalkingVelocity

                ( newState, newFramesSinceLastChain ) =
                    if framesOnWall < framesOnWallMaxDuration then
                        if controllerState.dPad == Right && not wallOnRight then
                            if controllerState.jump == Pressed then
                                let
                                    kickOffForce =
                                        defaultJumpForce
                                            |> V2.add ( kickOffHorizontalVelocity, 0 )
                                in
                                    ( Jumping kickOffForce, 0 )
                            else
                                ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )
                        else if controllerState.dPad == Left && wallOnRight then
                            if controllerState.jump == Pressed then
                                let
                                    kickOffForce =
                                        defaultJumpForce
                                            |> V2.add ( -kickOffHorizontalVelocity, 0 )
                                in
                                    ( Jumping kickOffForce, 0 )
                            else
                                ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )
                        else
                            ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )
                    else
                        ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )
            in
                ( newState, newFramesSinceLastChain )


stateAfterEnemyCollision : Maybe Collision2D.Side -> ( PlayerState, Int ) -> ( PlayerState, Int )
stateAfterEnemyCollision collision ( playerState, framesSinceLastChain ) =
    case collision of
        Nothing ->
            ( playerState, framesSinceLastChain )

        Just side ->
            case side of
                Collision2D.Top ->
                    case playerState of
                        Running ->
                            ( HitStun 0, maxChainDuration )

                        Jumping jumpForce ->
                            ( HitStun 0, maxChainDuration )

                        Falling ->
                            ( HitStun 0, maxChainDuration )

                        Dashing framesDashing ->
                            ( HitStun 0, maxChainDuration )

                        DashRecovery ( framesRecovering, onGround ) ->
                            ( HitStun 0, maxChainDuration )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( HitStun 0, maxChainDuration )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )

                Collision2D.Right ->
                    case playerState of
                        Running ->
                            ( HitStun 0, maxChainDuration )

                        Jumping jumpForce ->
                            ( HitStun 0, maxChainDuration )

                        Falling ->
                            ( HitStun 0, maxChainDuration )

                        Dashing ( framesDashing, direction ) ->
                            ( Dashing ( 0, direction ), 0 )

                        DashRecovery ( framesRecovering, onGround ) ->
                            ( HitStun 0, maxChainDuration )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( HitStun 0, maxChainDuration )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )

                Collision2D.Left ->
                    case playerState of
                        Running ->
                            ( HitStun 0, maxChainDuration )

                        Jumping jumpForce ->
                            ( HitStun 0, maxChainDuration )

                        Falling ->
                            ( HitStun 0, maxChainDuration )

                        Dashing ( framesDashing, direction ) ->
                            ( Dashing ( 0, direction ), 0 )

                        DashRecovery ( framesRecovering, onGround ) ->
                            ( HitStun 0, maxChainDuration )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( HitStun 0, maxChainDuration )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )

                Collision2D.Bottom ->
                    case playerState of
                        Running ->
                            ( Jumping defaultJumpForce, 0 )

                        Jumping jumpForce ->
                            ( Jumping defaultJumpForce, 0 )

                        Falling ->
                            ( Jumping defaultJumpForce, 0 )

                        Dashing ( framesDashing, direction ) ->
                            ( Dashing ( 0, direction ), 0 )

                        DashRecovery ( framesRecovering, onGround ) ->
                            ( Jumping defaultJumpForce, 0 )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )


stateAfterPlatformCollision : Maybe Collision2D.Side -> ( PlayerState, Int ) -> ( PlayerState, Int )
stateAfterPlatformCollision collision ( playerState, framesSinceLastChain ) =
    case collision of
        Nothing ->
            case playerState of
                Jumping jumpForce ->
                    ( Jumping jumpForce, framesSinceLastChain )

                Falling ->
                    ( Falling, framesSinceLastChain )

                Dashing ( framesDashing, direction ) ->
                    ( Dashing ( framesDashing, direction ), framesSinceLastChain )

                DashRecovery ( framesRecovering, onGround ) ->
                    ( DashRecovery ( framesRecovering, False ), framesSinceLastChain )

                HitStun framesHitStuned ->
                    ( HitStun framesHitStuned, maxChainDuration )

                Running ->
                    ( Falling, framesSinceLastChain )

                OnWall ( framesOnWall, wallOnRight ) ->
                    ( Falling, framesSinceLastChain )

        Just side ->
            case side of
                Collision2D.Top ->
                    ( playerState, framesSinceLastChain )

                Collision2D.Right ->
                    case playerState of
                        Running ->
                            ( Running, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( OnWall ( 0, True ), framesSinceLastChain )

                        Falling ->
                            ( OnWall ( 0, True ), framesSinceLastChain )

                        Dashing framesDashing ->
                            ( HitStun 0, maxChainDuration )

                        DashRecovery ( framesRecovering, onGround ) ->
                            if onGround then
                                ( DashRecovery ( framesRecovering, True ), maxChainDuration )
                            else
                                ( OnWall ( 0, True ), framesSinceLastChain )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )

                Collision2D.Left ->
                    case playerState of
                        Running ->
                            ( Running, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( OnWall ( 0, False ), framesSinceLastChain )

                        Falling ->
                            ( OnWall ( 0, False ), framesSinceLastChain )

                        Dashing framesDashing ->
                            ( HitStun 0, maxChainDuration )

                        DashRecovery ( framesRecovering, onGround ) ->
                            if onGround then
                                ( DashRecovery ( framesRecovering, True ), maxChainDuration )
                            else
                                ( OnWall ( 0, False ), framesSinceLastChain )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )

                Collision2D.Bottom ->
                    case playerState of
                        Running ->
                            ( Running, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( Running, framesSinceLastChain )

                        Falling ->
                            ( Running, framesSinceLastChain )

                        Dashing ( framesDashing, direction ) ->
                            ( Dashing ( framesDashing, direction ), framesSinceLastChain )

                        DashRecovery ( framesRecovering, onGround ) ->
                            if onGround then
                                ( DashRecovery ( framesRecovering, True ), maxChainDuration )
                            else
                                ( Running, framesSinceLastChain )

                        OnWall ( framesOnWall, wallOnRight ) ->
                            ( Running, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, maxChainDuration )


resetPlayerToOrigin : Vector -> Vector
resetPlayerToOrigin location =
    if V2.getY location < -1000 then
        ( 0, 0 )
    else
        location


capHorizontalVelocity : Float -> Vector -> Vector
capHorizontalVelocity maxSpeed ( x, y ) =
    if x > maxSpeed then
        ( maxSpeed, y )
    else if x < -maxSpeed then
        ( -maxSpeed, y )
    else
        ( x, y )


capVerticalVelocity : Float -> Vector -> Vector
capVerticalVelocity maxSpeed ( x, y ) =
    if y < -maxSpeed then
        ( x, -maxSpeed )
    else
        ( x, y )


renderPlayer : Resources -> Player -> Renderable
renderPlayer resources player =
    -- let
    --     ( x, y ) =
    --         player.location
    -- in
    --     Render.animatedSpriteWithOptions
    --         { position = ( x, y, 0 )
    --         , size = ( toFloat player.collisionRadius * 2, toFloat player.collisionRadius * 2 )
    --         , texture = Resources.getTexture "../assets/ghost-friend.png" resources
    --         , bottomLeft = ( 0, 0 )
    --         , topRight = ( 1, 1 )
    --         , duration = 1
    --         , numberOfFrames = 8
    --         , rotation = 0
    --         , pivot = ( 0.5, 0 )
    --         }
    let
        ( x, y ) =
            player.location

        color =
            case player.playerState of
                Running ->
                    Color.blue

                Jumping jumpForce ->
                    Color.green

                Falling ->
                    Color.darkGreen

                HitStun framesHitStuned ->
                    Color.red

                Dashing ( framesDashing, direction ) ->
                    Color.yellow

                DashRecovery ( framesRecovering, onGround ) ->
                    Color.purple

                OnWall ( framesOnWall, wallOnRight ) ->
                    if framesOnWall < framesOnWallMaxDuration then
                        Color.orange
                    else
                        Color.darkOrange

        newColor =
            if player.framesSinceLastChain < maxChainDuration then
                Color.black
            else
                color
    in
        Render.rectangle
            { color = newColor
            , position = centerToBottomLeftLocationConverter player.location player.size
            , size = player.size
            }
