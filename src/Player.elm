module Player exposing (Player, PlayerState(..), updatePlayer, renderPlayer, stateAfterPlatformCollision, stateAfterEnemyCollision)

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
    | Jumping
    | Falling
    | Dashing Float
    | OnWall ( Float, Bool )
    | HitStun Float


chainSpeedModifier : Float
chainSpeedModifier =
    1.5


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


updatePlayer : ControllerState -> Player -> Player
updatePlayer controller player =
    let
        gravitationalForce =
            gravity

        controllerDirectionalForce =
            if controller.dPad == Left then
                controllerLeftForce
            else if controller.dPad == Right then
                controllerRightForce
            else
                ( 0, 0 )

        modifiedControllerDirectionalForce =
            if player.framesSinceLastChain < maxChainDuration then
                controllerDirectionalForce
                    |> (\( x, y ) -> ( x * chainSpeedModifier, y ))
            else
                controllerDirectionalForce

        jumpForce =
            if controller.jump == Pressed then
                ( 0, 50 )
            else
                ( 0, 0 )
    in
        case player.playerState of
            Running ->
                let
                    newVelocity =
                        player.velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> V2.add jumpForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add player.location
                            |> resetPlayerToOrigin

                    newState =
                        if controller.dash == Pressed then
                            Dashing 0
                        else
                            Running

                    newerState =
                        if controller.jump == Pressed then
                            -- jumping cancels dashing
                            Jumping
                        else
                            newState
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , playerState = newerState
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }

            Jumping ->
                let
                    newVelocity =
                        player.velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add player.location
                            |> resetPlayerToOrigin
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }

            Falling ->
                let
                    newVelocity =
                        player.velocity
                            |> V2.add gravitationalForce
                            |> V2.add modifiedControllerDirectionalForce
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add player.location
                            |> resetPlayerToOrigin
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }

            Dashing framesDashing ->
                let
                    newVelocity =
                        if framesDashing < toFloat framesDashingMaxDuration then
                            player.velocity
                                |> V2.add jumpForce
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap
                        else
                            player.velocity
                                |> V2.add jumpForce
                                |> (\( x, y ) -> ( x * 0, y ))
                                |> capHorizontalVelocity speedCap
                                |> capVerticalVelocity speedCap

                    newLocation =
                        -- double velocity while dashing
                        newVelocity
                            |> (\( x, y ) -> ( x * 2, y ))
                            |> V2.add player.location
                            |> resetPlayerToOrigin

                    newState =
                        if controller.jump == Pressed then
                            -- jumping cancels dashing
                            Jumping
                        else
                            Dashing (framesDashing + 1)
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , playerState = newState
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }

            OnWall ( framesOnWall, wallOnRight ) ->
                let
                    ( newVelocity, newState ) =
                        if framesOnWall < toFloat framesOnWallMaxDuration then
                            if controller.dPad == Right && not wallOnRight then
                                if controller.jump == Pressed then
                                    let
                                        newVelocity =
                                            ( 0, 0 )
                                                |> V2.add gravitationalForce
                                                |> V2.add modifiedControllerDirectionalForce
                                                |> V2.add jumpForce
                                                |> (\( x, y ) -> ( x * resistance, y ))
                                                |> capHorizontalVelocity speedCap
                                                |> capVerticalVelocity speedCap

                                        newState =
                                            Jumping
                                    in
                                        ( newVelocity, newState )
                                else
                                    let
                                        newVelocity =
                                            ( 0, 0 )
                                                |> (\( x, y ) -> ( x * resistance, y ))
                                                |> capHorizontalVelocity speedCap
                                                |> capVerticalVelocity speedCap

                                        newState =
                                            OnWall ( framesOnWall + 1, wallOnRight )
                                    in
                                        ( newVelocity, newState )
                            else if controller.dPad == Left && wallOnRight then
                                if controller.jump == Pressed then
                                    let
                                        newVelocity =
                                            ( 0, 0 )
                                                |> V2.add gravitationalForce
                                                |> V2.add modifiedControllerDirectionalForce
                                                |> V2.add jumpForce
                                                |> (\( x, y ) -> ( x * resistance, y ))
                                                |> capHorizontalVelocity speedCap
                                                |> capVerticalVelocity speedCap

                                        newState =
                                            Jumping
                                    in
                                        ( newVelocity, newState )
                                else
                                    let
                                        newVelocity =
                                            ( 0, 0 )
                                                |> (\( x, y ) -> ( x * resistance, y ))
                                                |> capHorizontalVelocity speedCap
                                                |> capVerticalVelocity speedCap

                                        newState =
                                            OnWall ( framesOnWall + 1, wallOnRight )
                                    in
                                        ( newVelocity, newState )
                            else
                                let
                                    newVelocity =
                                        ( 0, 0 )
                                            |> (\( x, y ) -> ( x * resistance, y ))
                                            |> capHorizontalVelocity speedCap
                                            |> capVerticalVelocity speedCap

                                    newState =
                                        OnWall ( framesOnWall + 1, wallOnRight )
                                in
                                    ( newVelocity, newState )
                        else
                            let
                                newVelocity =
                                    ( 0, 0 )
                                        |> V2.add gravitationalForce
                                        |> V2.add modifiedControllerDirectionalForce
                                        |> (\( x, y ) -> ( x * resistance, y ))
                                        |> capHorizontalVelocity speedCap
                                        |> capVerticalVelocity speedCap

                                newState =
                                    OnWall ( framesOnWall + 1, wallOnRight )
                            in
                                ( newVelocity, newState )

                    newLocation =
                        newVelocity
                            |> V2.add player.location
                            |> resetPlayerToOrigin
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , playerState = newState
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }

            HitStun framesHitStuned ->
                let
                    newVelocity =
                        player.velocity
                            |> (\( x, y ) -> ( x * resistance, y ))
                            |> capHorizontalVelocity speedCap
                            |> capVerticalVelocity speedCap

                    newLocation =
                        newVelocity
                            |> V2.add player.location
                            |> resetPlayerToOrigin

                    state =
                        if framesHitStuned > toFloat hitStunMaxDuration then
                            Running
                        else
                            HitStun (framesHitStuned + 1)
                in
                    { player
                        | location = newLocation
                        , velocity = newVelocity
                        , playerState = state
                        , framesSinceLastChain = player.framesSinceLastChain + 1
                    }


stateAfterPlatformCollision : Maybe Collision2D.Side -> PlayerState -> PlayerState
stateAfterPlatformCollision collision playerState =
    case collision of
        Nothing ->
            case playerState of
                Jumping ->
                    Jumping

                Falling ->
                    Falling

                Dashing framesDashing ->
                    Dashing framesDashing

                HitStun framesHitStuned ->
                    HitStun framesHitStuned

                Running ->
                    Jumping

                OnWall framesOnWall ->
                    Jumping

        Just side ->
            case side of
                Collision2D.Top ->
                    playerState

                Collision2D.Right ->
                    case playerState of
                        Running ->
                            Running

                        Jumping ->
                            OnWall ( 0, True )

                        Falling ->
                            OnWall ( 0, True )

                        Dashing framesDashing ->
                            HitStun 0

                        OnWall framesOnWall ->
                            OnWall framesOnWall

                        HitStun framesHitStuned ->
                            HitStun framesHitStuned

                Collision2D.Left ->
                    case playerState of
                        Running ->
                            Running

                        Jumping ->
                            OnWall ( 0, False )

                        Falling ->
                            OnWall ( 0, False )

                        Dashing framesDashing ->
                            HitStun 0

                        OnWall framesOnWall ->
                            OnWall framesOnWall

                        HitStun framesHitStuned ->
                            HitStun framesHitStuned

                Collision2D.Bottom ->
                    case playerState of
                        Running ->
                            Running

                        Jumping ->
                            Running

                        Falling ->
                            Running

                        Dashing framesDashing ->
                            Dashing framesDashing

                        OnWall framesOnWall ->
                            Running

                        HitStun framesHitStuned ->
                            HitStun framesHitStuned


stateAfterEnemyCollision : Maybe Collision2D.Side -> Int -> PlayerState -> ( PlayerState, Int )
stateAfterEnemyCollision collision framesSinceLastChain playerState =
    case collision of
        Nothing ->
            ( playerState, framesSinceLastChain )

        Just side ->
            case side of
                Collision2D.Top ->
                    case playerState of
                        Running ->
                            ( HitStun 0, framesSinceLastChain )

                        Jumping ->
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

                        Jumping ->
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

                        Jumping ->
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
                            ( Jumping, 0 )

                        Jumping ->
                            ( Jumping, 0 )

                        Falling ->
                            ( Jumping, 0 )

                        Dashing framesDashing ->
                            ( Dashing 0, 0 )

                        OnWall framesOnWall ->
                            ( OnWall framesOnWall, framesSinceLastChain )

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

                Jumping ->
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
