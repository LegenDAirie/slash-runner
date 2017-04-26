module CustomEncoders exposing (encodeVector, levelDataEncodeHandler, encodeEnemy)

import Json.Encode
import GameTypes exposing (Vector)
import Coordinates exposing (pixelToGridConversion)
import GamePlatform exposing (Platform)
import Enemy exposing (Enemy)


encodeVector : Vector -> Json.Encode.Value
encodeVector location =
    let
        ( x, y ) =
            pixelToGridConversion location
    in
        Json.Encode.object
            [ ( "x", Json.Encode.float x )
            , ( "y", Json.Encode.float y )
            ]


levelDataEncodeHandler : List Platform -> List Enemy -> String
levelDataEncodeHandler platforms enemies =
    let
        encodedPlatforms =
            List.map (\platform -> encodePlatform platform) platforms

        newPlatforms =
            Json.Encode.list encodedPlatforms

        encodedEnemies =
            List.map (\enemy -> encodeEnemy enemy) enemies
                |> Json.Encode.list

        encodedlevelData =
            Json.Encode.object
                [ ( "platforms", newPlatforms )
                , ( "enemies", encodedEnemies )
                ]
    in
        Json.Encode.encode 4 encodedlevelData


encodePlatform : Platform -> Json.Encode.Value
encodePlatform platform =
    Json.Encode.object
        [ ( "location", encodeVector platform.location ) ]


encodeEnemy : Enemy -> Json.Encode.Value
encodeEnemy enemy =
    Json.Encode.object
        [ ( "location", encodeVector enemy.location )
        ]
