module CreateLevel exposing (..)

import Screens.NormalPlay exposing (NormalPlayState, initialNormalPlayState, renderNormalPlay)
import Game.TwoD.Render as Render exposing (Renderable)
import Keyboard.Extra
import GameTypes exposing (Vector)
import GamePlatform exposing (Platform, platformSize)
import MouseHelpers exposing (mouseToGridInPixels)
import Enemy exposing (Enemy, Movement(..))
import CustomEncoders exposing (encodeVector, levelDataEncodeHandler)
import GamePlatform exposing (Platform, platformSize)
import Color
import Coordinates exposing (centerToBottomLeftLocationConverter, gridSquareSize)


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
    | APlatform
    | AnEnemy


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
                APlatform
            else if List.member Keyboard.Extra.Number2 pressedKeys then
                AnEnemy
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

        newPlatform =
            Platform newPosition

        newPlatforms =
            case itemToPlace of
                PlaceNothing ->
                    playState.platforms

                AnEnemy ->
                    playState.platforms

                APlatform ->
                    if List.member newPlatform playState.platforms then
                        List.filter (\platform -> not (platform == newPlatform)) playState.platforms
                    else
                        [ Platform newPosition ]
                            |> List.append playState.platforms

        newEnemy =
            Enemy newPosition 0 ( 64, 64 ) NoMovement

        newEnemies =
            case itemToPlace of
                PlaceNothing ->
                    playState.permanentEnemies

                APlatform ->
                    playState.permanentEnemies

                AnEnemy ->
                    if List.member newEnemy.location (List.map (\enemy -> enemy.location) playState.permanentEnemies) then
                        List.filter (\enemy -> not (enemy.location == newEnemy.location)) playState.permanentEnemies
                    else
                        [ newEnemy ]
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

                APlatform ->
                    Color.charcoal

                AnEnemy ->
                    Color.red
    in
        Render.rectangle
            { color = mouseColor
            , position = centerToBottomLeftLocationConverter location gridSquareSize
            , size = gridSquareSize
            }
