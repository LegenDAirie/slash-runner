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
import Controller
import Coordinates
import CustomEncoders
import Dict
import Enemy
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render
import GameFeel
import GamePlatform
import Keyboard
import Keyboard.Arrows
import MouseHelpers exposing (mouseToGridInPixels)
import NormalPlay exposing (NormalPlayState, initialNormalPlayState, renderNormalPlay, resetPlayState, updateNormalPlay)
import V2


type alias LevelCreateState =
    { itemToPlace : ItemToBePlaced
    , cursorLocation : V2.Vector2
    , cursorActive : Bool
    , cameraLocation : V2.Vector2
    , camera : Camera
    , playState : NormalPlayState
    }


initialLevelCreateState : LevelCreateState
initialLevelCreateState =
    { itemToPlace = PlaceNothing
    , cursorLocation = ( 0, 0 )
    , cursorActive = False
    , playState = initialNormalPlayState
    , cameraLocation = V2.xyRecordToVector initialNormalPlayState.player
    , camera = Camera.fixedWidth (Tuple.first Coordinates.gameSize) (V2.xyRecordToVector initialNormalPlayState.player)
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
        if List.member (Keyboard.Character "0") pressedKeys then
            UpdateCursorItem PlaceNothing

        else if List.member (Keyboard.Character "1") pressedKeys then
            UpdateCursorItem ANormalPlatform

        else if List.member (Keyboard.Character "2") pressedKeys then
            UpdateCursorItem AStaticEnemy

        else if List.member (Keyboard.Character "3") pressedKeys then
            UpdateCursorItem ADangerousPlatform

        else if List.member (Keyboard.Character "4") pressedKeys then
            UpdateCursorItem AnEnemyOnTrack

        else if List.member (Keyboard.Character "5") pressedKeys then
            UpdateCursorItem AWalkingEnemy

        else if List.member (Keyboard.Character "9") pressedKeys then
            UpdateCursorItem Remove

        else if List.member (Keyboard.Character "R") pressedKeys then
            ResetAllTheThings

        else if List.member (Keyboard.Character "H") pressedKeys && List.member (Keyboard.Character "G") pressedKeys then
            RemoveAllTheThings

        else
            NoAction

    else
        NoAction


updateCreateLevelState : Controller.Controller -> V2.Vector2 -> List Keyboard.Key -> GameFeel.GameFeel -> LevelCreateState -> ( LevelCreateState, Maybe String )
updateCreateLevelState controller windowSize pressedKeys tempProperties levelCreateState =
    let
        ( newCreateLevelState, possibleEncodedLevelData ) =
            getCreateStateUpdateAction levelCreateState.playState pressedKeys
                |> actionUpdate levelCreateState
                |> updateCamera pressedKeys levelCreateState.playState.paused
                |> updatePlayStateFromMouseState windowSize pressedKeys
    in
    ( { newCreateLevelState
        | playState = updateNormalPlay controller tempProperties newCreateLevelState.playState
      }
    , possibleEncodedLevelData
    )


updateCamera : List Keyboard.Key -> Bool -> LevelCreateState -> LevelCreateState
updateCamera pressedKeys paused state =
    case paused of
        True ->
            { state
                | cameraLocation = updateCameraLocation pressedKeys state.cameraLocation
                , camera = Camera.follow 0.5 0.17 (updateCameraLocation pressedKeys state.cameraLocation) state.camera
            }

        False ->
            { state
                | camera = state.playState.camera
                , cameraLocation = V2.xyRecordToVector state.playState.player
            }


updateCameraLocation : List Keyboard.Key -> V2.Vector2 -> V2.Vector2
updateCameraLocation pressedKeys ( x, y ) =
    case Keyboard.Arrows.wasdDirection pressedKeys of
        Keyboard.Arrows.North ->
            ( x, y + 10 )

        Keyboard.Arrows.NorthEast ->
            ( x + 10, y + 10 )

        Keyboard.Arrows.East ->
            ( x + 10, y )

        Keyboard.Arrows.SouthEast ->
            ( x + 10, y - 10 )

        Keyboard.Arrows.South ->
            ( x, y - 10 )

        Keyboard.Arrows.SouthWest ->
            ( x - 10, y - 10 )

        Keyboard.Arrows.West ->
            ( x - 10, y )

        Keyboard.Arrows.NorthWest ->
            ( x - 10, y + 10 )

        Keyboard.Arrows.NoDirection ->
            ( x, y )


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


updatePlayStateFromMouseState : V2.Vector2 -> List Keyboard.Key -> LevelCreateState -> ( LevelCreateState, Maybe String )
updatePlayStateFromMouseState windowSize pressedKeys levelCreateState =
    -- holy shit I have no fucking idea what I was thinking with this one...
    let
        ( width, height ) =
            Coordinates.gridSquareSize

        { itemToPlace, playState, cursorLocation, cursorActive } =
            levelCreateState

        newPosition =
            mouseToGridInPixels windowSize levelCreateState.camera cursorLocation

        newNormalPlatform =
            GamePlatform.Normal

        newDangerousPlatform =
            GamePlatform.Dangerous

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
            V2.vectorIntToFloat newPosition

        newStaticEnemy =
            Enemy.Enemy floatPosition 0 ( 64, 64 ) Enemy.NoMovement True

        startNode =
            V2.add floatPosition ( -128, 0 )

        endNode =
            V2.add floatPosition ( 128, 0 )

        newEnemyOnTrack =
            Enemy.Enemy floatPosition 0 ( 64, 64 ) (Enemy.LinePath (Enemy.LineMovementSpec startNode endNode floatPosition 1)) True

        newWalkingEnemy =
            Enemy.Enemy floatPosition 0 ( 64, 64 ) (Enemy.Walk floatPosition) True

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
            if List.member Keyboard.Shift pressedKeys && List.member (Keyboard.Character "S") pressedKeys then
                Just (CustomEncoders.levelDataEncodeHandler newPlayState.platforms newPlayState.permanentEnemies)

            else
                Nothing

        newLevelCreateState =
            { levelCreateState
                | playState = newPlayState
            }
    in
    ( newLevelCreateState, encodedLevelData )



--------------------------------------------------------------------------------
-------------------------------- Render ----------------------------------------


renderLevelCreateScreen : V2.Vector2 -> LevelCreateState -> List Render.Renderable
renderLevelCreateScreen windowSize levelCreateState =
    let
        { itemToPlace, cursorLocation, playState } =
            levelCreateState

        newMouseLocation =
            mouseToGridInPixels windowSize levelCreateState.camera cursorLocation
    in
    List.concat
        [ [ renderCursorBlock itemToPlace (V2.vectorIntToFloat newMouseLocation) ]
        , renderNormalPlay playState
        ]


renderCursorBlock : ItemToBePlaced -> V2.Vector2 -> Render.Renderable
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
        , size = V2.vectorIntToFloat Coordinates.gridSquareSize
        }
