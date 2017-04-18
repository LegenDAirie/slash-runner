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
    | Dashing Float
    | OnWall ( Float, Bool )
    | HitStun Float


defaultJumpForce : Vector
defaultJumpForce =
    ( 0, 50 )


jumpDampening : Float
jumpDampening =
    0.9


chainSpeedModifier : Float
chainSpeedModifier =
    10


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


updatePlayer : List Enemy -> List Wall -> ControllerState -> Player -> Player
updatePlayer enemies walls controllerState player =
    let
        ( newLocation, newVelocity ) =
            applyPhysics controllerState.dPad player.playerState player.framesSinceLastChain player.location player.velocity

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
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add jumpForce
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
            if framesDashing < toFloat framesDashingMaxDuration then
                ( Dashing (framesDashing + 1), framesSinceLastChain + 1 )
            else
                ( Dashing (framesDashing + 1), maxChainDuration )

        OnWall ( framesOnWall, wallOnRight ) ->
            if framesOnWall < toFloat framesOnWallMaxDuration then
                ( OnWall ( framesOnWall + 1, wallOnRight ), framesSinceLastChain + 1 )
            else
                ( OnWall ( framesOnWall + 1, wallOnRight ), maxChainDuration )

        HitStun framesHitStuned ->
            if framesHitStuned < toFloat hitStunMaxDuration then
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
                    ( Jumping ( getX jumpForce, getY jumpForce * jumpDampening ), framesSinceLastChain )

                Released ->
                    ( Jumping ( 0, 0 ), framesSinceLastChain )

                Inactive ->
                    ( Jumping ( 0, 0 ), framesSinceLastChain )

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
                            ( HitStun 0, maxChainDuration )

                        Jumping jumpForce ->
                            ( HitStun 0, maxChainDuration )

                        Falling ->
                            ( HitStun 0, maxChainDuration )

                        Dashing framesDashing ->
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

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

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

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

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

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

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

                Dashing framesDashing ->
                    ( Dashing framesDashing, framesSinceLastChain )

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

                        Dashing framesDashing ->
                            ( Dashing framesDashing, framesSinceLastChain )

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
