module GameFeel exposing
    ( GameFeel
    , Msg
    , gameFeelView
    , initialGameFeel
    , updateGameFeel
    )

import Html
import Html.Attributes
import Html.Events


type alias GameFeel =
    -- that jump feel
    { framesToApex : Int
    , maxJumpHeight : Float
    , minJumpHeight : Float
    , maxWallSlideSpeed : Float
    , maxWalkingSpeed : Float
    , maxRunningSpeed : Float
    , maxDashingSpeed : Float
    , dPadAcceleration : Float
    , dashDuration : Int
    , buttonPressWindow : Int
    }


initialGameFeel : GameFeel
initialGameFeel =
    { framesToApex = 28
    , maxJumpHeight = 320
    , minJumpHeight = 16
    , maxWallSlideSpeed = 3
    , maxWalkingSpeed = 10
    , maxRunningSpeed = 20
    , maxDashingSpeed = 25
    , dPadAcceleration = 0.5
    , dashDuration = 41
    , buttonPressWindow = 13
    }


type Msg
    = GotNewJumpDuration Int
    | GotNewMaxJumpHeight Float
    | GotNewMinJumpHeight Float
    | GotNewMaxWallSlideSpeed Float
    | GotNewMaxWalkingSpeed Float
    | GotNewMaxRunningSpeed Float
    | GotNewMaxDashingSpeed Float
    | GotNewDPadAcceleration Float
    | GotNewDashDuration Int
    | GotNewButtonPressWindow Int


updateGameFeel : Msg -> GameFeel -> GameFeel
updateGameFeel msg gameFeel =
    case msg of
        GotNewJumpDuration framesToApex ->
            { gameFeel | framesToApex = framesToApex }

        GotNewMaxJumpHeight maxJumpHeight ->
            { gameFeel | maxJumpHeight = maxJumpHeight }

        GotNewMinJumpHeight minJumpHeight ->
            { gameFeel | minJumpHeight = minJumpHeight }

        GotNewMaxWallSlideSpeed maxWallSlideSpeed ->
            { gameFeel | maxWallSlideSpeed = maxWallSlideSpeed }

        GotNewMaxWalkingSpeed maxWalkingSpeed ->
            { gameFeel | maxWalkingSpeed = maxWalkingSpeed }

        GotNewMaxDashingSpeed maxDashingSpeed ->
            { gameFeel | maxDashingSpeed = maxDashingSpeed }

        GotNewMaxRunningSpeed maxRunningSpeed ->
            { gameFeel | maxRunningSpeed = maxRunningSpeed }

        GotNewDPadAcceleration dPadAcceleration ->
            { gameFeel | dPadAcceleration = dPadAcceleration }

        GotNewDashDuration dashDuration ->
            { gameFeel | dashDuration = dashDuration }

        GotNewButtonPressWindow buttonPressWindow ->
            { gameFeel | buttonPressWindow = buttonPressWindow }


gameFeelView : GameFeel -> Html.Html Msg
gameFeelView gameFeel =
    Html.div []
        [ Html.div []
            [ Html.text "Frames to Apex"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "128"
                , Html.Attributes.min "1"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromInt gameFeel.framesToApex)
                , Html.Events.onInput (\stringNumber -> GotNewJumpDuration <| clamp 1 128 <| Maybe.withDefault 0 (String.toInt stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Max Jumping Height"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "512"
                , Html.Attributes.min (String.fromFloat gameFeel.minJumpHeight)
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat gameFeel.maxJumpHeight)
                , Html.Events.onInput (\stringNumber -> GotNewMaxJumpHeight <| clamp gameFeel.minJumpHeight 512 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Min Jumping Height"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max (String.fromFloat gameFeel.maxJumpHeight)
                , Html.Attributes.min "16"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat gameFeel.minJumpHeight)
                , Html.Events.onInput (\stringNumber -> GotNewMinJumpHeight <| clamp 16 gameFeel.maxJumpHeight <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Max Wall Slide Speed "
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "10"
                , Html.Attributes.min "0"
                , Html.Attributes.step "0.5"
                , Html.Attributes.value (String.fromFloat gameFeel.maxWallSlideSpeed)
                , Html.Events.onInput (\stringNumber -> GotNewMaxWallSlideSpeed <| clamp 0 10 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Max Walking Speed"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "25"
                , Html.Attributes.min "5"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat gameFeel.maxWalkingSpeed)
                , Html.Events.onInput (\stringNumber -> GotNewMaxWalkingSpeed <| clamp 5 25 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Max Running Speed"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "50"
                , Html.Attributes.min <| String.fromFloat gameFeel.maxWalkingSpeed
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat gameFeel.maxRunningSpeed)
                , Html.Events.onInput (\stringNumber -> GotNewMaxRunningSpeed <| clamp gameFeel.maxWalkingSpeed 50 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Max Dashing Speed"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "50"
                , Html.Attributes.min <| String.fromFloat gameFeel.maxRunningSpeed
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromFloat gameFeel.maxDashingSpeed)
                , Html.Events.onInput (\stringNumber -> GotNewMaxDashingSpeed <| clamp gameFeel.maxRunningSpeed 50 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "DPad Acceleration"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "3"
                , Html.Attributes.min "0.1"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value (String.fromFloat gameFeel.dPadAcceleration)
                , Html.Events.onInput (\stringNumber -> GotNewDPadAcceleration <| clamp 0.1 3 <| Maybe.withDefault 0 (String.toFloat stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Dash Duration in frames"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "50"
                , Html.Attributes.min "25"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromInt gameFeel.dashDuration)
                , Html.Events.onInput (\stringNumber -> GotNewDashDuration <| clamp 25 50 <| Maybe.withDefault 0 (String.toInt stringNumber))
                ]
                []
            ]
        , Html.div []
            [ Html.text "Dash button press window duration"
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.max "18"
                , Html.Attributes.min "8"
                , Html.Attributes.step "1"
                , Html.Attributes.value (String.fromInt gameFeel.buttonPressWindow)
                , Html.Events.onInput (\stringNumber -> GotNewButtonPressWindow <| clamp 8 18 <| Maybe.withDefault 0 (String.toInt stringNumber))
                ]
                []
            ]
        ]
