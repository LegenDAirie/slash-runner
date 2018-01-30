module Controller
    exposing
        ( ControllerState
        , calculateControllerStateFromKeyboardState
        , initialControllerState
        , calculateControllerStateFromGamePad
        , GamePadState
        , ButtonState
            ( Pressed
            , Held
            , Released
            , Inactive
            )
        , DPad
            ( Up
            , UpRight
            , Right
            , DownRight
            , Down
            , DownLeft
            , Left
            , UpLeft
            , NoDirection
            )
        )

import Keyboard.Extra


type alias GamePadState =
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


calculateControllerStateFromKeyboardState : Keyboard.Extra.State -> ControllerState -> ControllerState
calculateControllerStateFromKeyboardState keyboardState controllerState =
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


calculateControllerStateFromGamePad : GamePadState -> ControllerState -> ControllerState
calculateControllerStateFromGamePad gamePad controllerState =
    let
        direction =
            if gamePad.up then
                if gamePad.right then
                    UpRight
                else if gamePad.left then
                    UpLeft
                else
                    Up
            else if gamePad.down then
                if gamePad.right then
                    DownRight
                else if gamePad.left then
                    DownLeft
                else
                    Down
            else if gamePad.left then
                Left
            else if gamePad.right then
                Right
            else
                NoDirection
    in
        { controllerState
            | dPad = direction
            , jump = calculateButtonState gamePad.jump controllerState.jump
            , dash = calculateButtonState gamePad.dash controllerState.dash
        }
