module Enemy exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Vector, vectorDecoder)
import Coordinates exposing (centerToBottomLeftLocationConverter)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)


type alias Enemy =
    { location : Vector
    , timeExisted : Int
    , size : Vector
    }


updateEnemies : List Enemy -> List Enemy
updateEnemies enemies =
    List.map updateEnemy enemies


updateEnemy : Enemy -> Enemy
updateEnemy enemy =
    let
        newTimeExisted =
            enemy.timeExisted + 1
    in
        { enemy
            | timeExisted = newTimeExisted
            , location = enemy.location
        }


renderEnemy : Enemy -> Renderable
renderEnemy enemy =
    let
        x =
            getX enemy.location

        y =
            getY enemy.location
    in
        Render.rectangle
            { color = Color.red
            , position = centerToBottomLeftLocationConverter enemy.location enemy.size
            , size = enemy.size
            }


enemyDecoder : Decoder Enemy
enemyDecoder =
    decode Enemy
        |> required "location" vectorDecoder
        |> hardcoded 0
        |> hardcoded ( 64, 64 )
