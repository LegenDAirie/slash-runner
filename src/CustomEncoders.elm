module CustomEncoders exposing (encodeEnemy, encodeVector, levelDataEncodeHandler)

import Coordinates exposing (pixelToGridConversion)
import Dict exposing (Dict)
import Enemy
    exposing
        ( Enemy
        , EnemyMovement(..)
        )
import GamePlatform
import Json.Encode
import V2



-- having all of the encoders here might be a good idea.


encodeVector : V2.Vector2 -> Json.Encode.Value
encodeVector location =
    let
        ( x, y ) =
            pixelToGridConversion location
    in
    Json.Encode.object
        [ ( "x", Json.Encode.float x )
        , ( "y", Json.Encode.float y )
        ]


encodeGridCoordinate : V2.IntVector -> Json.Encode.Value
encodeGridCoordinate gridCoordinate =
    let
        ( x, y ) =
            gridCoordinate
    in
    Json.Encode.object
        [ ( "x", Json.Encode.int x )
        , ( "y", Json.Encode.int y )
        ]


levelDataEncodeHandler : Dict V2.IntVector GamePlatform.Platform -> List Enemy -> String
levelDataEncodeHandler platforms enemies =
    let
        encodedPlatforms =
            platforms
                |> Dict.toList

        newPlatforms =
            Json.Encode.list (\( gridCoordinate, platform ) -> encodePlatformAndLocation gridCoordinate platform) encodedPlatforms

        encodedEnemies =
            Json.Encode.list (\enemy -> encodeEnemy enemy) enemies

        encodedlevelData =
            Json.Encode.object
                [ ( "platforms", newPlatforms )
                , ( "enemies", encodedEnemies )
                ]
    in
    Json.Encode.encode 4 encodedlevelData


encodePlatformAndLocation : V2.IntVector -> GamePlatform.Platform -> Json.Encode.Value
encodePlatformAndLocation location platform =
    Json.Encode.object
        [ ( "location", encodeGridCoordinate location )
        , ( "platform", encodePlatform platform )
        ]


encodePlatform : GamePlatform.Platform -> Json.Encode.Value
encodePlatform platform =
    Json.Encode.object
        [ ( "platformType", encodePlatformType platform )
        ]


encodeEnemy : Enemy -> Json.Encode.Value
encodeEnemy enemy =
    Json.Encode.object
        [ ( "location", encodeVector enemy.startingLocation )
        , ( "movement", encodeMovement enemy.movement )
        ]


encodeMovement : EnemyMovement -> Json.Encode.Value
encodeMovement movement =
    case movement of
        NoMovement ->
            Json.Encode.string "NoMovement"

        Walk currentLocation ->
            Json.Encode.string "Walk"

        LinePath lineMovementSpec ->
            Json.Encode.object
                [ ( "startNode", encodeVector lineMovementSpec.startNode )
                , ( "endNode", encodeVector lineMovementSpec.endNode )
                , ( "speed", Json.Encode.float lineMovementSpec.speed )
                ]


encodePlatformType : GamePlatform.Platform -> Json.Encode.Value
encodePlatformType platform =
    case platform of
        GamePlatform.Normal ->
            Json.Encode.string "Normal"

        GamePlatform.Dangerous ->
            Json.Encode.string "Dangerous"
