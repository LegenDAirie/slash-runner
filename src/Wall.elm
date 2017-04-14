module Wall exposing (Wall, renderWall)

import Color
import GameTypes exposing (Vector)
import Coordinates exposing (centerToBottomLeftLocationConverter)
import Game.TwoD.Render as Render exposing (Renderable)


type alias Wall =
    { location : Vector
    , size : Vector
    }


renderWall : Wall -> Renderable
renderWall wall =
    let
        ( x, y ) =
            wall.location
    in
        Render.rectangle
            { color = Color.charcoal
            , position = centerToBottomLeftLocationConverter wall.location wall.size
            , size = wall.size
            }
