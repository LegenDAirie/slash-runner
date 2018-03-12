module Controller
    exposing
        ( Controller
        , calculateControllerStateFromKeyboardState
        , initialControllerState
        , calculateControllerStateFromGamePad
        , GamePad
        , DPadHorizontal(DPadRight, DPadLeft, NoHorizontalDPad)
        , DPadVertical(DPadUp, DPadDown, NoVerticalDPad)
        , ButtonState(Pressed, Held, Released, Inactive)
        )

import Keyboard.Extra


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


type alias Controller =
    { dPadHorizontal : DPadHorizontal
    , dPadVertical : DPadVertical
    , jump : ButtonState
    , dash : ButtonState
    , start : ButtonState
    }


type alias DPad dPad =
    { dPad
        | dPadHorizontal : DPadHorizontal
        , dPadVertical : DPadVertical
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
    , jump = Inactive
    , dash = Inactive
    , start = Inactive
    }


calculateControllerStateFromKeyboardState : Keyboard.Extra.State -> Controller -> Controller
calculateControllerStateFromKeyboardState keyboardState controllerState =
    let
        pressedKeys =
            Keyboard.Extra.pressedDown keyboardState

        jumpPressed =
            List.any (\key -> key == Keyboard.Extra.CharL) pressedKeys

        dashPressed =
            List.any (\key -> key == Keyboard.Extra.CharK) pressedKeys

        startPressed =
            List.any (\key -> key == Keyboard.Extra.CharP) pressedKeys

        wasd =
            Keyboard.Extra.wasdDirection keyboardState

        ( horizontalDirection, verticalDirection ) =
            case wasd of
                Keyboard.Extra.North ->
                    ( NoHorizontalDPad, DPadUp )

                Keyboard.Extra.NorthEast ->
                    ( DPadRight, DPadUp )

                Keyboard.Extra.East ->
                    ( DPadRight, NoVerticalDPad )

                Keyboard.Extra.SouthEast ->
                    ( DPadRight, DPadDown )

                Keyboard.Extra.South ->
                    ( NoHorizontalDPad, DPadDown )

                Keyboard.Extra.SouthWest ->
                    ( DPadLeft, DPadDown )

                Keyboard.Extra.West ->
                    ( DPadLeft, NoVerticalDPad )

                Keyboard.Extra.NorthWest ->
                    ( DPadLeft, DPadUp )

                Keyboard.Extra.NoDirection ->
                    ( NoHorizontalDPad, NoVerticalDPad )
    in
        { controllerState
            | dPadHorizontal = horizontalDirection
            , dPadVertical = verticalDirection
            , jump = calculateButtonState jumpPressed controllerState.jump
            , dash = calculateButtonState dashPressed controllerState.dash
            , start = calculateButtonState startPressed controllerState.start
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
            , jump = calculateButtonState gamePad.jump controllerState.jump
            , dash = calculateButtonState gamePad.dash controllerState.dash
        }



------------------------- controller helpers -------------------------------------
