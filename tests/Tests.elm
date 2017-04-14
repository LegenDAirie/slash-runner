module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import GameTypes exposing (..)
import Button exposing (ButtonState(..))
import GameLogic exposing (hasPlayerCollided, touchIsCollidingWithBarrel, areAnyBarrelsInTheWay)
import Barrel exposing (nearestPiOverFour)
import Screens.LevelCreationScreen exposing (isScreenBeingScrolled)


all : Test
all =
    describe "Game"
        [ describe "A Test Suite"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "collision detection"
            [ test "collision" <|
                \() ->
                    let
                        player =
                            Player ( 2, 3 ) ( 0, 0 ) 1

                        barrel =
                            Barrel ( 3, 2 ) (pi / 4) 1 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement
                    in
                        Expect.true "Expected collision" (hasPlayerCollided player barrel)
            , test "collision" <|
                \() ->
                    let
                        player =
                            Player ( 7, 4 ) ( 0, 0 ) 1

                        barrel =
                            Barrel ( 9, 3 ) (pi / 4) 1 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement
                    in
                        Expect.false "Expected no collision" (hasPlayerCollided player barrel)
            ]
        , describe "Game Logic Helper Functions"
            [ test "touch colliding with barrel" <|
                \() ->
                    Barrel ( 3, 2 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement
                        |> touchIsCollidingWithBarrel ( 3, 2 )
                        |> Expect.true "Expected a barrel in the way"
            , test "touchs colliding with barrels" <|
                \() ->
                    let
                        touches =
                            [ ( 3, 2 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 4, 2 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrelTwo =
                            Barrel ( 0, 0 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrels =
                            [ barrelOne, barrelTwo ]
                    in
                        Expect.true "Expected barrels in the way" (areAnyBarrelsInTheWay touches barrels)
            , test "no collisions" <|
                \() ->
                    let
                        touches =
                            [ ( 4, 7 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 20, 20 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrelTwo =
                            Barrel ( 30, 30 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrels =
                            [ barrelOne, barrelTwo ]
                    in
                        Expect.false "Expected no barrels in the way" (areAnyBarrelsInTheWay touches barrels)
            , test "scroll Screen Button being touched" <|
                \() ->
                    let
                        touches =
                            [ ( 4, 7 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 20, 20 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrelTwo =
                            Barrel ( 30, 30 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrels =
                            [ barrelOne, barrelTwo ]

                        editModeButtons =
                            { switchToPlayTestMode = Inactive
                            , addBarrelButton = Inactive
                            }
                    in
                        isScreenBeingScrolled touches barrels editModeButtons
                            |> Expect.true "Scroll Screen button being touched"
            , test "scroll Screen Button being touched" <|
                \() ->
                    let
                        touches =
                            [ ( 4, 7 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 20, 20 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrelTwo =
                            Barrel ( 30, 30 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrels =
                            [ barrelOne, barrelTwo ]

                        editModeButtons =
                            { switchToPlayTestMode = Held
                            , addBarrelButton = Inactive
                            }
                    in
                        isScreenBeingScrolled touches barrels editModeButtons
                            |> Expect.false "switchToPlayTestMode being held should fail test"
            , test "scroll Screen Button being touched" <|
                \() ->
                    let
                        touches =
                            [ ( 20, 20 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 20, 20 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrelTwo =
                            Barrel ( 30, 30 ) 0 2 0 (NoRotation (NoRotationSpec (ManualFire False))) NoMovement

                        barrels =
                            [ barrelOne, barrelTwo ]

                        editModeButtons =
                            { switchToPlayTestMode = Inactive
                            , addBarrelButton = Inactive
                            }
                    in
                        isScreenBeingScrolled touches barrels editModeButtons
                            |> Expect.false "Barrel in the way should fail test"
            ]
        , describe "Nearest Pi Over Four"
            [ test "Nearest pi/4 of pi/2.1 is pi/2" <|
                \() ->
                    nearestPiOverFour (pi / 2.1)
                        |> Expect.equal (pi / 2)
            , test "Nearest pi/4 of pi/1.9 is pi/2" <|
                \() ->
                    nearestPiOverFour (pi / 1.9)
                        |> Expect.equal (pi / 2)
            , test "Nearest pi/4 of 0 is 0" <|
                \() ->
                    nearestPiOverFour (pi / 0)
                        |> Expect.equal (pi / 0)
            , test "Nearest pi/4 of 3.2pi/2 is 3pi/2" <|
                \() ->
                    nearestPiOverFour (3.2 * pi / 2)
                        |> Expect.equal (3 * pi / 2)
            , test "Nearest pi/4 of 3.25pi/2 is 7pi/4" <|
                \() ->
                    nearestPiOverFour (3.25 * pi / 2)
                        |> Expect.equal (7 * pi / 4)
            ]
        ]
