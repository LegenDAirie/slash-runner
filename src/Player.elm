module Player exposing (renderPlayer)

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import GameTypes exposing (Player)
import Coordinates exposing (centerToBottomLeftLocationConverter)


renderPlayer : Resources -> Player -> Renderable
renderPlayer resources player =
    Render.shape
        Render.rectangle
        { color = Color.blue
        , position = centerToBottomLeftLocationConverter player.location player.size
        , size = player.size
        }
