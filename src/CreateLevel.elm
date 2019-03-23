module CreateLevel exposing
    ( LevelCreateState
    , initialLevelCreateState
    , renderLevelCreateScreen
    , updateCreateLevelState
    )

-- System
-- Libraries
-- my modules

import Color
import Controller exposing (Controller)
import Coordinates exposing (gridSquareSize)
import CustomEncoders exposing (levelDataEncodeHandler)
import Dict
import Enemy exposing (Enemy, EnemyMovement(..), LineMovementSpec)
import Game.TwoD.Render as Render exposing (Renderable)
import GamePlatform exposing (Platform, PlatformType(..), platformSize)
import GameTypes exposing (TempProperties, Vector, vectorIntToFloat)
import Keyboard
import MouseHelpers exposing (mouseToGridInPixels)
import Screens.NormalPlay exposing (NormalPlayState, initialNormalPlayState, renderNormalPlay, resetPlayState, updateNormalPlay)
import V2


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


getCreateStateUpdateAction : NormalPlayState -> List Keyboard.Key -> CreateLevelAction
getCreateStateUpdateAction playState pressedKeys =
    if playState.paused then
        if List.member (Keyboard.Character "48") pressedKeys then
            UpdateCursorItem PlaceNothing

        else if List.member (Keyboard.Character "49") pressedKeys then
            UpdateCursorItem ANormalPlatform

        else if List.member (Keyboard.Character "50") pressedKeys then
            UpdateCursorItem AStaticEnemy

        else if List.member (Keyboard.Character "51") pressedKeys then
            UpdateCursorItem ADangerousPlatform

        else if List.member (Keyboard.Character "52") pressedKeys then
            UpdateCursorItem AnEnemyOnTrack

        else if List.member (Keyboard.Character "53") pressedKeys then
            UpdateCursorItem AWalkingEnemy

        else if List.member (Keyboard.Character "57") pressedKeys then
            UpdateCursorItem Remove

        else if List.member (Keyboard.Character "114") pressedKeys then
            ResetAllTheThings

        else if List.member (Keyboard.Character "104") pressedKeys && List.member (Keyboard.Character "103") pressedKeys then
            RemoveAllTheThings

        else
            NoAction

    else
        NoAction


updateCreateLevelState : Controller -> Vector -> List Keyboard.Key -> TempProperties -> LevelCreateState -> ( LevelCreateState, Maybe String )
updateCreateLevelState controller windowSize pressedKeys tempProperties levelCreateState =
    -- everything in this function could just be in main but I am waiting till
    -- I get the update logic working again and then implement extensible record types
    let
        ( newCreateLevelState, possibleEncodedLevelData ) =
            getCreateStateUpdateAction levelCreateState.playState pressedKeys
                |> actionUpdate levelCreateState
                |> updatePlayStateFromMouseState windowSize pressedKeys
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


updatePlayStateFromMouseState : Vector -> List Keyboard.Key -> LevelCreateState -> ( LevelCreateState, Maybe String )
updatePlayStateFromMouseState windowSize pressedKeys levelCreateState =
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

        encodedLevelData =
            if List.member Keyboard.Shift pressedKeys && List.member (Keyboard.Character "115") pressedKeys then
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
