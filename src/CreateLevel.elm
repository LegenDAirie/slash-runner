module CreateLevel
    exposing
        ( LevelCreateState
        , initialLevelCreateState
        , renderLevelCreateScreen
        , updateCreateLevelState
        )

-- System

import Color
import Dict


-- Libraries

import Game.TwoD.Render as Render exposing (Renderable)
import Keyboard.Extra
import Vector2 as V2
import Keyboard.Extra


-- my modules

import Screens.NormalPlay exposing (NormalPlayState, updateNormalPlay, initialNormalPlayState, renderNormalPlay, resetPlayState)
import Coordinates exposing (gridSquareSize)
import CustomEncoders exposing (levelDataEncodeHandler)
import GamePlatform exposing (Platform, platformSize, PlatformType(Normal, Dangerous))
import GameTypes exposing (Vector, TempProperties, vectorIntToFloat)
import MouseHelpers exposing (mouseToGridInPixels)
import Enemy exposing (Enemy, EnemyMovement(NoMovement, LinePath, Walk), LineMovementSpec)
import Controller exposing (Controller)


type alias LevelCreateState =
    { itemToPlace : ItemToBePlaced
    , cursorLocation : Vector
    , cursorActive : Bool
    , locationForCameraToFollow : Vector
    , playState : NormalPlayState
    }


initialLevelCreateState : LevelCreateState
initialLevelCreateState =
    { itemToPlace = PlaceNothing
    , cursorLocation = ( 0, 0 )
    , cursorActive = False
    , playState = initialNormalPlayState
    , locationForCameraToFollow = ( 0, 0 )
    }


type ItemToBePlaced
    = PlaceNothing
    | ANormalPlatform
    | ADangerousPlatform
    | AStaticEnemy
    | AnEnemyOnTrack
    | AWalkingEnemy
    | Remove


type CreateLevelAction
    = ResetAllTheThings
    | RemoveAllTheThings
    | UpdateCursorItem ItemToBePlaced
    | NoAction



-- | SameLevelData


getCreateStateUpdateAction : NormalPlayState -> List Keyboard.Extra.Key -> CreateLevelAction
getCreateStateUpdateAction playState pressedKeys =
    if playState.paused then
        if List.member Keyboard.Extra.Number0 pressedKeys then
            UpdateCursorItem PlaceNothing
        else if List.member Keyboard.Extra.Number1 pressedKeys then
            UpdateCursorItem ANormalPlatform
        else if List.member Keyboard.Extra.Number2 pressedKeys then
            UpdateCursorItem AStaticEnemy
        else if List.member Keyboard.Extra.Number3 pressedKeys then
            UpdateCursorItem ADangerousPlatform
        else if List.member Keyboard.Extra.Number4 pressedKeys then
            UpdateCursorItem AnEnemyOnTrack
        else if List.member Keyboard.Extra.Number5 pressedKeys then
            UpdateCursorItem AWalkingEnemy
        else if List.member Keyboard.Extra.Number9 pressedKeys then
            UpdateCursorItem Remove
        else if List.member Keyboard.Extra.CharR pressedKeys then
            ResetAllTheThings
        else if List.member Keyboard.Extra.CharH pressedKeys && List.member Keyboard.Extra.CharG pressedKeys then
            RemoveAllTheThings
        else
            NoAction
    else
        NoAction


updateCreateLevelState : Controller -> Vector -> Keyboard.Extra.State -> TempProperties -> LevelCreateState -> ( LevelCreateState, Maybe String )
updateCreateLevelState controller windowSize keyboard tempProperties levelCreateState =
    -- everything in this function could just be in main but I am waiting till
    -- I get the update logic working again and then implement extensible record types
    let
        ( newCreateLevelState, possibleEncodedLevelData ) =
            getCreateStateUpdateAction levelCreateState.playState (Keyboard.Extra.pressedDown keyboard)
                |> actionUpdate levelCreateState
                |> updatePlayStateFromMouseState windowSize keyboard
    in
        ( { newCreateLevelState
            | playState = updateNormalPlay controller tempProperties newCreateLevelState.playState
          }
        , possibleEncodedLevelData
        )


actionUpdate : LevelCreateState -> CreateLevelAction -> LevelCreateState
actionUpdate levelCreateState action =
    case action of
        ResetAllTheThings ->
            { levelCreateState
                | playState = resetPlayState levelCreateState.playState
            }

        RemoveAllTheThings ->
            { levelCreateState
                | playState = initialNormalPlayState
            }

        UpdateCursorItem itemToBePlaced ->
            { levelCreateState
                | itemToPlace = itemToBePlaced
            }

        NoAction ->
            levelCreateState


updatePlayStateFromMouseState : Vector -> Keyboard.Extra.State -> LevelCreateState -> ( LevelCreateState, Maybe String )
updatePlayStateFromMouseState windowSize keyboardState levelCreateState =
    -- holy shit I have no fucking idea what I was thinking with this one...
    let
        ( width, height ) =
            platformSize

        { itemToPlace, playState, cursorLocation, cursorActive } =
            levelCreateState

        newPosition =
            mouseToGridInPixels windowSize playState.camera cursorLocation

        newNormalPlatform =
            Platform Normal

        newDangerousPlatform =
            Platform Dangerous

        newPlatforms =
            case itemToPlace of
                PlaceNothing ->
                    playState.platforms

                Remove ->
                    case cursorActive of
                        True ->
                            Dict.remove newPosition playState.platforms

                        False ->
                            playState.platforms

                AStaticEnemy ->
                    playState.platforms

                AnEnemyOnTrack ->
                    playState.platforms

                AWalkingEnemy ->
                    playState.platforms

                ANormalPlatform ->
                    case cursorActive of
                        True ->
                            if Dict.member newPosition playState.platforms then
                                playState.platforms
                            else
                                Dict.insert newPosition newNormalPlatform playState.platforms

                        False ->
                            playState.platforms

                ADangerousPlatform ->
                    if Dict.member newPosition playState.platforms then
                        Dict.remove newPosition playState.platforms
                    else
                        Dict.insert newPosition newDangerousPlatform playState.platforms

        floatPosition =
            vectorIntToFloat newPosition

        newStaticEnemy =
            Enemy floatPosition 0 ( 64, 64 ) NoMovement True

        startNode =
            V2.add floatPosition ( -128, 0 )

        endNode =
            V2.add floatPosition ( 128, 0 )

        newEnemyOnTrack =
            Enemy floatPosition 0 ( 64, 64 ) (LinePath (LineMovementSpec startNode endNode floatPosition 1)) True

        newWalkingEnemy =
            Enemy floatPosition 0 ( 64, 64 ) (Walk floatPosition) True

        newEnemies =
            case itemToPlace of
                PlaceNothing ->
                    playState.permanentEnemies

                Remove ->
                    playState.permanentEnemies

                ANormalPlatform ->
                    playState.permanentEnemies

                ADangerousPlatform ->
                    playState.permanentEnemies

                AStaticEnemy ->
                    if List.member newStaticEnemy.startingLocation (List.map (\enemy -> enemy.startingLocation) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.startingLocation == newStaticEnemy.startingLocation)) playState.permanentEnemies
                    else
                        [ newStaticEnemy ]
                            |> List.append playState.permanentEnemies

                AnEnemyOnTrack ->
                    if List.member newEnemyOnTrack.startingLocation (List.map (\enemy -> enemy.startingLocation) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.startingLocation == newEnemyOnTrack.startingLocation)) playState.permanentEnemies
                    else
                        [ newEnemyOnTrack ]
                            |> List.append playState.permanentEnemies

                AWalkingEnemy ->
                    if List.member newWalkingEnemy.startingLocation (List.map (\enemy -> enemy.startingLocation) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.startingLocation == newWalkingEnemy.startingLocation)) playState.permanentEnemies
                    else
                        [ newWalkingEnemy ]
                            |> List.append playState.permanentEnemies

        newPlayState =
            { playState
                | platforms = newPlatforms
                , enemies = newEnemies
                , permanentEnemies = newEnemies
            }

        pressedKeys =
            Keyboard.Extra.pressedDown keyboardState

        encodedLevelData =
            if List.member Keyboard.Extra.Shift pressedKeys && List.member Keyboard.Extra.CharS pressedKeys then
                Just (levelDataEncodeHandler newPlayState.platforms newPlayState.permanentEnemies)
            else
                Nothing

        newLevelCreateState =
            { levelCreateState
                | playState = newPlayState
            }
    in
        ( newLevelCreateState, encodedLevelData )


renderLevelCreateScreen : Vector -> LevelCreateState -> List Renderable
renderLevelCreateScreen windowSize levelCreateState =
    let
        { itemToPlace, cursorLocation, playState } =
            levelCreateState

        newMouseLocation =
            mouseToGridInPixels windowSize playState.camera cursorLocation
    in
        List.concat
            [ [ renderCursorBlock itemToPlace (vectorIntToFloat newMouseLocation) ]
            , renderNormalPlay playState
            ]


renderCursorBlock : ItemToBePlaced -> Vector -> Renderable
renderCursorBlock itemToBePlace location =
    let
        mouseColor =
            case itemToBePlace of
                PlaceNothing ->
                    Color.black

                Remove ->
                    Color.brown

                ANormalPlatform ->
                    Color.charcoal

                ADangerousPlatform ->
                    Color.yellow

                AStaticEnemy ->
                    Color.red

                AnEnemyOnTrack ->
                    Color.orange

                AWalkingEnemy ->
                    Color.purple
    in
        Render.shape
            Render.rectangle
            { color = mouseColor
            , position = location
            , size = vectorIntToFloat gridSquareSize
            }
