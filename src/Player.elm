module Player exposing (renderPlayer, getPlayerLeftKickPoint, getPlayerRightKickPoint)

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Player, Vector, vectorIntToFloat)


getPlayerLeftKickPoint : Player -> Vector
getPlayerLeftKickPoint player =
    let
        { hitBoxSize, spriteSize, location } =
            player

        spriteHitBoxSizeDif =
            getX spriteSize - getX hitBoxSize

        spriteBoxLeftSide =
            getX location - toFloat spriteHitBoxSizeDif / 2

        kickPointY =
            ((toFloat <| getY player.hitBoxSize) / 2) + getY player.location
    in
        ( spriteBoxLeftSide, kickPointY )


getPlayerRightKickPoint : Player -> Vector
getPlayerRightKickPoint player =
    let
        { hitBoxSize, spriteSize, location } =
            player

        spriteHitBoxSizeDif =
            getX spriteSize - getX hitBoxSize

        spriteBoxRightSide =
            toFloat spriteHitBoxSizeDif / 2 + (toFloat <| getX player.hitBoxSize) + getX player.location

        kickPointY =
            ((toFloat <| getY player.hitBoxSize) / 2) + getY player.location
    in
        ( spriteBoxRightSide, kickPointY )


renderPlayer : Resources -> Player -> List Renderable
renderPlayer resources player =
    let
        hitBox =
            Render.shape
                Render.rectangle
                { color = Color.blue
                , position = player.location
                , size = vectorIntToFloat player.hitBoxSize
                }

        fullSprite =
            Render.sprite
                { texture = Resources.getTexture "./assets/player-background-glow.png" resources
                , position = V2.sub player.location ( 32, 32 )
                , size = vectorIntToFloat player.spriteSize
                }
    in
        [ fullSprite
        , hitBox
        ]
