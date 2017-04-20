module Controller exposing (ButtonState(..), calculateButtonState, DPad(..), ControllerState, calculateControllerState, initialControllerState)

import Keyboard.Extra


type ButtonState
    = Pressed
    | Held
    | Released
    | Inactive


calculateButtonState : Bool -> ButtonState -> ButtonState
calculateButtonState isPressed currentButtonState =
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



--------------------------------------------------------------------------------
--Controller
--------------------------------------------------------------------------------


type alias ControllerState =
    { dPad : DPad
    , jump : ButtonState
    , dash : ButtonState
    , start : ButtonState
    }


type DPad
    = Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft
    | NoDirection


initialControllerState : ControllerState
initialControllerState =
    { dPad = NoDirection
    , jump = Inactive
    , dash = Inactive
    , start = Inactive
    }


calculateControllerState : Keyboard.Extra.State -> ControllerState -> ControllerState
calculateControllerState keyboardState controllerState =
    let
        pressedKeys =
            Keyboard.Extra.pressedDown keyboardState

        jumpPressed =
            List.any (\key -> key == Keyboard.Extra.CharL) pressedKeys

        dashPressed =
            List.any (\key -> key == Keyboard.Extra.CharK) pressedKeys

        wasd =
            Keyboard.Extra.wasdDirection keyboardState

        direction =
            case wasd of
                Keyboard.Extra.North ->
                    Up

                Keyboard.Extra.NorthEast ->
                    UpRight

                Keyboard.Extra.East ->
                    Right

                Keyboard.Extra.SouthEast ->
                    DownRight

                Keyboard.Extra.South ->
                    Down

                Keyboard.Extra.SouthWest ->
                    DownLeft

                Keyboard.Extra.West ->
                    Left

                Keyboard.Extra.NorthWest ->
                    UpLeft

                Keyboard.Extra.NoDirection ->
                    NoDirection
    in
        { controllerState
            | dPad = direction
            , jump = calculateButtonState jumpPressed controllerState.jump
            , dash = calculateButtonState dashPressed controllerState.dash
        }
