module GamePlatform exposing (Platform, renderPlatform, platformSize, platformDecoder, PlatformType(..))

import Color
import GameTypes exposing (Vector, vectorDecoder)
import Coordinates exposing (centerToBottomLeftLocationConverter, gridSquareSize)
import Game.TwoD.Render as Render exposing (Renderable)
import Game.Resources as Resources exposing (Resources)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Platform =
    { location : Vector
    , platformType : PlatformType
    }


type PlatformType
    = Normal
    | Dangerous


platformSize : Vector
platformSize =
    gridSquareSize


renderPlatform : Resources -> Platform -> Renderable
renderPlatform resources platform =
    let
        ( x, y ) =
            platform.location

        resource =
            case platform.platformType of
                Normal ->
                    "../assets/tile-bricks-test.png"

                Dangerous ->
                    "../assets/tile-bricks-test.png"
    in
        Render.spriteWithOptions
            { position = ( x, y, 0 )
            , size = platformSize
            , texture = Resources.getTexture resource resources
            , rotation = 0
            , pivot = ( 0.5, 0.5 )
            , tiling = ( 1, 1 )
            }



-- renderPlatform : Resources -> Platform -> Renderable
-- renderPlatform resources platform =
--     let
--         ( x, y ) =
--             platform.location
--
--         color =
--             case platform.platformType of
--                 Normal ->
--                     Color.charcoal
--
--                 Dangerous ->
--                     Color.yellow
--     in
--         Render.shape
--             Render.rectangle
--             { color = color
--             , position = centerToBottomLeftLocationConverter platform.location platformSize
--             , size = platformSize
--             }


platformDecoder : Decoder Platform
platformDecoder =
    decode Platform
        |> required "location" vectorDecoder
        |> required "platformType" platformTypeDecoder


platformTypeDecoder : Decoder PlatformType
platformTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen stringToPlatformType


stringToPlatformType : String -> Decoder PlatformType
stringToPlatformType platformType =
    case platformType of
        "Dangerous" ->
            Json.Decode.succeed Dangerous

        _ ->
            Json.Decode.succeed Normal
