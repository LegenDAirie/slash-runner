module Controller exposing
    ( ButtonState(..)
    , Controller
    , DPadHorizontal(..)
    , DPadVertical(..)
    , GamePad
    , calculateControllerStateFromGamePad
    , calculateControllerStateFromKeyboardState
    , initialControllerState
    , isButtonDown
    )

import Keyboard
import Keyboard.Arrows


type alias GamePad =
    { gamepadConnected : Bool
    , up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    , jump : Bool
    , dash : Bool
    }


type ButtonState
    = Pressed
    | Held
    | Released
    | Inactive


updateButtonState : Bool -> ButtonState -> ButtonState
updateButtonState isPressed currentButtonState =
    case isPressed of
        True ->
            case currentButtonState of
                Pressed ->
                    Held

                Held ->
                    Held

                Released ->
                    Pressed

                Inactive ->
                    Pressed

        False ->
            case currentButtonState of
                Pressed ->
                    Released

                Held ->
                    Released

                Released ->
                    Inactive

                Inactive ->
                    Inactive



----------------------------------
-- Helpers
----------------------------------


isButtonDown : ButtonState -> Bool
isButtonDown buttonState =
    case buttonState of
        Pressed ->
            True

        Held ->
            True

        Released ->
            False

        Inactive ->
            False



--------------------------------------------------------------------------------
--Controller
--------------------------------------------------------------------------------


type alias Controller =
    { dPadHorizontal : DPadHorizontal
    , dPadVertical : DPadVertical
    , jumpButton : ButtonState
    , dashButton : ButtonState
    , startButton : ButtonState
    }


type DPadHorizontal
    = DPadLeft
    | DPadRight
    | NoHorizontalDPad


type DPadVertical
    = DPadUp
    | DPadDown
    | NoVerticalDPad


initialControllerState : Controller
initialControllerState =
    { dPadHorizontal = NoHorizontalDPad
    , dPadVertical = NoVerticalDPad
    , jumpButton = Inactive
    , dashButton = Inactive
    , startButton = Inactive
    }


calculateControllerStateFromKeyboardState : List Keyboard.Key -> Controller -> Controller
calculateControllerStateFromKeyboardState pressedKeys controllerState =
    let
        charL =
            Keyboard.Character "108"

        jumpPressed =
            List.any (\key -> key == charL) pressedKeys

        charK =
            Keyboard.Character "107"

        dashPressed =
            List.any (\key -> key == charK) pressedKeys

        charP =
            Keyboard.Character "112"

        startPressed =
            List.any (\key -> key == charP) pressedKeys

        wasd =
            Keyboard.Arrows.wasdDirection pressedKeys

        ( horizontalDirection, verticalDirection ) =
            case wasd of
                Keyboard.Arrows.North ->
                    ( NoHorizontalDPad, DPadUp )

                Keyboard.Arrows.NorthEast ->
                    ( DPadRight, DPadUp )

                Keyboard.Arrows.East ->
                    ( DPadRight, NoVerticalDPad )

                Keyboard.Arrows.SouthEast ->
                    ( DPadRight, DPadDown )

                Keyboard.Arrows.South ->
                    ( NoHorizontalDPad, DPadDown )

                Keyboard.Arrows.SouthWest ->
                    ( DPadLeft, DPadDown )

                Keyboard.Arrows.West ->
                    ( DPadLeft, NoVerticalDPad )

                Keyboard.Arrows.NorthWest ->
                    ( DPadLeft, DPadUp )

                Keyboard.Arrows.NoDirection ->
                    ( NoHorizontalDPad, NoVerticalDPad )
    in
    { controllerState
        | dPadHorizontal = horizontalDirection
        , dPadVertical = verticalDirection
        , jumpButton = updateButtonState jumpPressed controllerState.jumpButton
        , dashButton = updateButtonState dashPressed controllerState.dashButton
        , startButton = updateButtonState startPressed controllerState.startButton
    }


calculateControllerStateFromGamePad : GamePad -> Controller -> Controller
calculateControllerStateFromGamePad gamePad controllerState =
    let
        ( horizontalDirection, verticalDirection ) =
            if gamePad.up then
                if gamePad.right then
                    ( DPadRight, DPadUp )

                else if gamePad.left then
                    ( DPadLeft, DPadUp )

                else
                    ( NoHorizontalDPad, DPadUp )

            else if gamePad.down then
                if gamePad.right then
                    ( DPadRight, DPadDown )

                else if gamePad.left then
                    ( DPadLeft, DPadDown )

                else
                    ( NoHorizontalDPad, DPadDown )

            else if gamePad.left then
                ( DPadLeft, NoVerticalDPad )

            else if gamePad.right then
                ( DPadRight, NoVerticalDPad )

            else
                ( NoHorizontalDPad, NoVerticalDPad )
    in
    { controllerState
        | dPadHorizontal = horizontalDirection
        , dPadVertical = verticalDirection
        , jumpButton = updateButtonState gamePad.jump controllerState.jumpButton
        , dashButton = updateButtonState gamePad.dash controllerState.dashButton
    }
