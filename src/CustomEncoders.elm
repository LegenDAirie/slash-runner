module CustomEncoders exposing (encodeVector, levelDataEncodeHandler)

import Json.Encode
import GameTypes exposing (Vector)
import Coordinates exposing (pixelToGridConversion)
import Wall exposing (Wall)


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


levelDataEncodeHandler : List Wall -> String
levelDataEncodeHandler walls =
    let
        encodedWalls =
            List.map (\wall -> encodeWall wall) walls

        platforms =
            Json.Encode.list encodedWalls

        encodedlevelData =
            Json.Encode.object
                [ ( "platforms", platforms ) ]
    in
        Json.Encode.encode 4 encodedlevelData


encodeWall : Wall -> Json.Encode.Value
encodeWall wall =
    Json.Encode.object
        [ ( "location", encodeVector wall.location ) ]
