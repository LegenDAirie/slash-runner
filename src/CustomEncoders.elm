module CustomEncoders exposing (encodeVector, levelDataEncodeHandler)

import Json.Encode
import GameTypes exposing (Vector)
import Coordinates exposing (pixelToGridConversion)
import GamePlatform exposing (Platform)


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


levelDataEncodeHandler : List Platform -> String
levelDataEncodeHandler platforms =
    let
        encodedPlatforms =
            List.map (\platform -> encodePlatform platform) platforms

        newPlatforms =
            Json.Encode.list encodedPlatforms

        encodedlevelData =
            Json.Encode.object
                [ ( "platforms", newPlatforms ) ]
    in
        Json.Encode.encode 4 encodedlevelData


encodePlatform : Platform -> Json.Encode.Value
encodePlatform platform =
    Json.Encode.object
        [ ( "location", encodeVector platform.location ) ]
