module CreateLevel exposing (..)

import Screens.NormalPlay exposing (NormalPlayState, initialNormalPlayState, renderNormalPlay)
import Game.TwoD.Render as Render exposing (Renderable)
import Keyboard.Extra
import GameTypes exposing (Vector)
import GamePlatform exposing (Platform, platformSize)
import MouseHelpers exposing (mouseToGridInPixels)
import Enemy exposing (Enemy, Movement(..), LineMovementSpec)
import CustomEncoders exposing (encodeVector, levelDataEncodeHandler)
import GamePlatform exposing (Platform, platformSize, PlatformType(..))
import Color
import Coordinates exposing (centerToBottomLeftLocationConverter, gridSquareSize)
import Vector2 as V2 exposing (getX, getY)


type alias LevelCreateState =
    { itemToPlace : ItemToPlace
    , mouseLocation : Vector
    , playState : NormalPlayState
    }


initialLevelCreateState : LevelCreateState
initialLevelCreateState =
    { itemToPlace = PlaceNothing
    , mouseLocation = ( 0, 0 )
    , playState = initialNormalPlayState
    }


type ItemToPlace
    = PlaceNothing
    | ANormalPlatform
    | ADangerousPlatform
    | AStaticEnemy
    | AnEnemyOnTrack


updatePlayStateAfterKeyPress : Keyboard.Extra.State -> LevelCreateState -> LevelCreateState
updatePlayStateAfterKeyPress keyboardState levelCreateState =
    let
        { itemToPlace, mouseLocation, playState } =
            levelCreateState

        pressedKeys =
            Keyboard.Extra.pressedDown keyboardState

        newEnemies =
            if List.member Keyboard.Extra.CharR pressedKeys then
                playState.permanentEnemies
            else if List.member Keyboard.Extra.CharH pressedKeys && List.member Keyboard.Extra.CharG pressedKeys then
                []
            else
                playState.enemies

        newPlatforms =
            if List.member Keyboard.Extra.CharH pressedKeys && List.member Keyboard.Extra.CharG pressedKeys then
                []
            else
                playState.platforms

        newNormalPlayState =
            { playState
                | enemies = newEnemies
                , platforms = newPlatforms
            }

        newItemToPlace =
            if List.member Keyboard.Extra.Number0 pressedKeys then
                PlaceNothing
            else if List.member Keyboard.Extra.Number1 pressedKeys then
                ANormalPlatform
            else if List.member Keyboard.Extra.Number2 pressedKeys then
                AStaticEnemy
            else if List.member Keyboard.Extra.Number3 pressedKeys then
                ADangerousPlatform
            else if List.member Keyboard.Extra.Number4 pressedKeys then
                AnEnemyOnTrack
            else
                itemToPlace

        _ =
            Debug.log "newItemToPlace" newItemToPlace
    in
        LevelCreateState newItemToPlace mouseLocation newNormalPlayState


updatePlayStateAfterMouseClick : Vector -> Vector -> Keyboard.Extra.State -> LevelCreateState -> ( LevelCreateState, String )
updatePlayStateAfterMouseClick canvasSize mousePosition keyboardState levelCreateState =
    let
        ( width, height ) =
            platformSize

        { itemToPlace, playState } =
            levelCreateState

        newPosition =
            mouseToGridInPixels canvasSize playState.camera mousePosition

        newNormalPlatform =
            Platform newPosition Normal

        newDangerousPlatform =
            Platform newPosition Dangerous

        newPlatforms =
            case itemToPlace of
                PlaceNothing ->
                    playState.platforms

                AStaticEnemy ->
                    playState.platforms

                AnEnemyOnTrack ->
                    playState.platforms

                ANormalPlatform ->
                    if List.member newNormalPlatform playState.platforms then
                        List.filter (\platform -> not (platform == newNormalPlatform)) playState.platforms
                    else
                        [ newNormalPlatform ]
                            |> List.append playState.platforms

                ADangerousPlatform ->
                    if List.member newDangerousPlatform playState.platforms then
                        List.filter (\platform -> not (platform == newDangerousPlatform)) playState.platforms
                    else
                        [ newDangerousPlatform ]
                            |> List.append playState.platforms

        newStaticEnemy =
            Enemy newPosition 0 ( 64, 64 ) NoMovement

        startNode =
            V2.add newPosition ( -128, 0 )

        endNode =
            V2.add newPosition ( 128, 0 )

        newEnemyOnTrack =
            Enemy newPosition 0 ( 64, 64 ) (LinePath (LineMovementSpec startNode endNode True 1))

        newEnemies =
            case itemToPlace of
                PlaceNothing ->
                    playState.permanentEnemies

                ANormalPlatform ->
                    playState.permanentEnemies

                ADangerousPlatform ->
                    playState.permanentEnemies

                AStaticEnemy ->
                    if List.member newStaticEnemy.location (List.map (\enemy -> enemy.location) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.location == newStaticEnemy.location)) playState.permanentEnemies
                    else
                        [ newStaticEnemy ]
                            |> List.append playState.permanentEnemies

                AnEnemyOnTrack ->
                    if List.member newEnemyOnTrack.location (List.map (\enemy -> enemy.location) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.location == newEnemyOnTrack.location)) playState.permanentEnemies
                    else
                        [ newEnemyOnTrack ]
                            |> List.append playState.permanentEnemies

        newPlayState =
            { playState
                | platforms = newPlatforms
                , enemies = newEnemies
                , permanentEnemies = newEnemies
            }

        encodedLevelData =
            levelDataEncodeHandler newPlayState.platforms newPlayState.enemies

        newLevelCreateState =
            { levelCreateState
                | playState = newPlayState
            }
    in
        ( newLevelCreateState, encodedLevelData )


renderLevelCreateScreen : LevelCreateState -> List Renderable
renderLevelCreateScreen levelCreateState =
    let
        { itemToPlace, mouseLocation, playState } =
            levelCreateState
    in
        List.concat
            [ [ renderMouse itemToPlace mouseLocation ]
            , (renderNormalPlay playState)
            ]


renderMouse : ItemToPlace -> Vector -> Renderable
renderMouse itemToPlace location =
    let
        mouseColor =
            case itemToPlace of
                PlaceNothing ->
                    Color.black

                ANormalPlatform ->
                    Color.charcoal

                ADangerousPlatform ->
                    Color.yellow

                AStaticEnemy ->
                    Color.red

                AnEnemyOnTrack ->
                    Color.orange
    in
        Render.shape
            Render.rectangle
            { color = mouseColor
            , position = centerToBottomLeftLocationConverter location gridSquareSize
            , size = gridSquareSize
            }
