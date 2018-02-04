module Player exposing (renderPlayer)

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import GameTypes exposing (Player, vectorIntToFloat)


renderPlayer : Resources -> Player -> Renderable
renderPlayer resources player =
    Render.shape
        Render.rectangle
        { color = Color.blue
        , position = player.location
        , size = vectorIntToFloat player.size
        }
