module Player exposing (Player, PlayerState(..), applyPhysics, renderPlayer, stateAfterPlatformCollision, stateAfterEnemyCollision, incrementPlayerCounters, stateAfterControllerInputs)

import Vector2 as V2 exposing (getX, getY)
import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import Collision2D
import GameTypes exposing (Vector)
import Coordinates exposing (centerToBottomLeftLocationConverter)
import Controller exposing (DPad(..), ControllerState, ButtonState(..))
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)


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
    | Dashing Float
    | OnWall ( Float, Bool )
    | HitStun Float


defaultJumpForce : Vector
defaultJumpForce =
    ( 0, 50 )


chainSpeedModifier : Float
chainSpeedModifier =
    3


maxChainDuration : Int
maxChainDuration =
    180


framesDashingMaxDuration : Int
framesDashingMaxDuration =
    15


framesOnWallMaxDuration : Int
framesOnWallMaxDuration =
    60


hitStunMaxDuration : Int
hitStunMaxDuration =
    60


applyPhysics : DPad -> PlayerState -> Int -> Vector -> Vector -> ( Vector, Vector )
applyPhysics dPad playerState framesSinceLastChain location velocity =
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

        modifiedControllerDirectionalForce =
            if framesSinceLastChain < maxChainDuration then
                controllerDirectionalForce
                    |> (\( x, y ) -> ( x * chainSpeedModifier, y ))
            else
                controllerDirectionalForce
    in
        case playerState of
            Running ->
                let
                    newVelocity =
                        velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Jumping jumpForce ->
                let
                    newVelocity =
                        velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> V2.add jumpForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Falling ->
                let
                    newVelocity =
                        velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            Dashing framesDashing ->
                let
                    newVelocity =
                        if framesDashing < toFloat framesDashingMaxDuration then
                            velocity
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap
                        else
                            velocity
                                |> (\( x, y ) -> ( x * 0, y ))
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap

                    newLocation =
                        -- double velocity while dashing
                        newVelocity
                            |> (\( x, y ) -> ( x * 2, y ))
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            OnWall ( framesOnWall, wallOnRight ) ->
                let
                    newVelocity =
                        if framesOnWall < toFloat framesOnWallMaxDuration then
                            ( 0, 0 )
                                |> (\( x, y ) -> ( x * resistance, y ))
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap
                        else
                            ( 0, 0 )
                                |> V2.add gravitationalForce
                                |> V2.add modifiedControllerDirectionalForce
                                |> (\( x, y ) -> ( x * resistance, y ))
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )

            HitStun framesHitStuned ->
                let
                    newVelocity =
                        velocity
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add location
                            |> resetPlayerToOrigin
                in
                    ( newLocation, newVelocity )


incrementPlayerCounters : ( PlayerState, Int ) -> ( PlayerState, Int )
incrementPlayerCounters ( playerState, framesSinceLastChain ) =
    case playerState of
        Running ->
            ( Running, framesSinceLastChain + 1 )

        Jumping jumpForce ->
            ( Jumping jumpForce, framesSinceLastChain + 1 )

        Falling ->
            ( Falling, framesSinceLastChain + 1 )

        Dashing framesDashing ->
            ( Dashing (framesDashing + 1), framesSinceLastChain + 1 )

        OnWall ( framesOnWall, wallOnRight ) ->
            ( OnWall ( framesOnWall + 1, wallOnRight ), framesSinceLastChain + 1 )

        HitStun framesHitStuned ->
            if framesHitStuned < toFloat hitStunMaxDuration then
                ( HitStun (framesHitStuned + 1), framesSinceLastChain + 1 )
            else
                ( Falling, framesSinceLastChain + 1 )


stateAfterControllerInputs : ControllerState -> ( PlayerState, Int ) -> ( PlayerState, Int )
stateAfterControllerInputs controllerState ( playerState, framesSinceLastChain ) =
    case playerState of
        HitStun framesHitStuned ->
            ( HitStun framesHitStuned, framesSinceLastChain )

        Falling ->
            ( Falling, framesSinceLastChain )

        Jumping jumpForce ->
            ( Jumping jumpForce, framesSinceLastChain )

        Running ->
            let
                newState =
                    if controllerState.dash == Pressed then
                        Dashing 0
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

        OnWall ( framesOnWall, wallOnRight ) ->
            let
                ( newState, newFramesSinceLastChain ) =
                    if framesOnWall < toFloat framesOnWallMaxDuration then
                        if controllerState.dPad == Right && not wallOnRight then
                            if controllerState.jump == Pressed then
                                ( Jumping defaultJumpForce, 0 )
                            else
                                ( OnWall ( framesOnWall, wallOnRight ), framesSinceLastChain )
                        else if controllerState.dPad == Left && wallOnRight then
                            if controllerState.jump == Pressed then
                                ( Jumping defaultJumpForce, 0 )
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
                            ( HitStun 0, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( HitStun 0, framesSinceLastChain )

                        Falling ->
                            ( HitStun 0, framesSinceLastChain )

                        Dashing framesDashing ->
                            ( HitStun 0, framesSinceLastChain )

                        OnWall framesOnWall ->
                            ( HitStun 0, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )

                Collision2D.Right ->
                    case playerState of
                        Running ->
                            ( HitStun 0, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( HitStun 0, framesSinceLastChain )

                        Falling ->
                            ( HitStun 0, framesSinceLastChain )

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

                        OnWall framesOnWall ->
                            ( HitStun 0, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )

                Collision2D.Left ->
                    case playerState of
                        Running ->
                            ( HitStun 0, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( HitStun 0, framesSinceLastChain )

                        Falling ->
                            ( HitStun 0, framesSinceLastChain )

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

                        OnWall framesOnWall ->
                            ( HitStun 0, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )

                Collision2D.Bottom ->
                    case playerState of
                        Running ->
                            ( Jumping defaultJumpForce, 0 )

                        Jumping jumpForce ->
                            ( Jumping defaultJumpForce, 0 )

                        Falling ->
                            ( Jumping defaultJumpForce, 0 )

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

                        OnWall framesOnWall ->
                            ( OnWall framesOnWall, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )


stateAfterPlatformCollision : Maybe Collision2D.Side -> ( PlayerState, Int ) -> ( PlayerState, Int )
stateAfterPlatformCollision collision ( playerState, framesSinceLastChain ) =
    case collision of
        Nothing ->
            case playerState of
                Jumping jumpForce ->
                    ( Jumping jumpForce, framesSinceLastChain )

                Falling ->
                    ( Falling, framesSinceLastChain )

                Dashing framesDashing ->
                    ( Dashing framesDashing, framesSinceLastChain )

                HitStun framesHitStuned ->
                    ( HitStun framesHitStuned, framesSinceLastChain )

                Running ->
                    ( Falling, framesSinceLastChain )

                OnWall framesOnWall ->
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
                            ( HitStun 0, framesSinceLastChain )

                        OnWall framesOnWall ->
                            ( OnWall framesOnWall, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )

                Collision2D.Left ->
                    case playerState of
                        Running ->
                            ( Running, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( OnWall ( 0, False ), framesSinceLastChain )

                        Falling ->
                            ( OnWall ( 0, False ), framesSinceLastChain )

                        Dashing framesDashing ->
                            ( HitStun 0, framesSinceLastChain )

                        OnWall framesOnWall ->
                            ( OnWall framesOnWall, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )

                Collision2D.Bottom ->
                    case playerState of
                        Running ->
                            ( Running, framesSinceLastChain )

                        Jumping jumpForce ->
                            ( Running, framesSinceLastChain )

                        Falling ->
                            ( Running, framesSinceLastChain )

                        Dashing framesDashing ->
                            ( Dashing framesDashing, framesSinceLastChain )

                        OnWall framesOnWall ->
                            ( Running, framesSinceLastChain )

                        HitStun framesHitStuned ->
                            ( HitStun framesHitStuned, framesSinceLastChain )


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

                Dashing framesDashing ->
                    if framesDashing < toFloat framesDashingMaxDuration then
                        Color.yellow
                    else
                        Color.darkYellow

                OnWall ( framesOnWall, wallOnRight ) ->
                    if framesOnWall < toFloat framesOnWallMaxDuration then
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
