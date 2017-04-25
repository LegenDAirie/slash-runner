module Enemy exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (..)
import Coordinates exposing (centerToBottomLeftLocationConverter)


type alias Enemy =
    { location : Vector
    , timeExisted : Float
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
