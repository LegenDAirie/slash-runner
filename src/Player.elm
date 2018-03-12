module Player exposing (renderPlayer, getPlayerLeftKickPoint, getPlayerRightKickPoint)

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Player, Vector, vectorIntToFloat)


getPlayerLeftKickPoint : Player -> Vector
getPlayerLeftKickPoint player =
    let
        { hitBoxSize, spriteSize, x, y } =
            player

        spriteHitBoxSizeDif =
            getX spriteSize - getX hitBoxSize

        spriteBoxLeftSide =
            x - toFloat spriteHitBoxSizeDif / 2

        kickPointY =
            ((toFloat <| getY player.hitBoxSize) / 2) + y
    in
        ( spriteBoxLeftSide, kickPointY )


getPlayerRightKickPoint : Player -> Vector
getPlayerRightKickPoint player =
    let
        { hitBoxSize, spriteSize } =
            player

        spriteHitBoxSizeDif =
            getX spriteSize - getX hitBoxSize

        spriteBoxRightSide =
            toFloat spriteHitBoxSizeDif / 2 + (toFloat <| getX player.hitBoxSize) + player.x

        kickPointY =
            ((toFloat <| getY player.hitBoxSize) / 2) + player.y
    in
        ( spriteBoxRightSide, kickPointY )


renderPlayer : Resources -> Player -> List Renderable
renderPlayer resources player =
    let
        { x, y } =
            player

        hitBox =
            Render.shape
                Render.rectangle
                { color = Color.blue
                , position = ( x, y )
                , size = vectorIntToFloat player.hitBoxSize
                }

        fullSprite =
            Render.sprite
                { texture = Resources.getTexture "./assets/player-background-glow.png" resources
                , position = ( x - 32, y - 32 )
                , size = vectorIntToFloat player.spriteSize
                }
    in
        [ fullSprite
        , hitBox
        ]
