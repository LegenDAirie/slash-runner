module Player exposing (renderPlayer, getPlayerLeftKickPoint, getPlayerRightKickPoint, playerSpriteSize, playerHitBoxSize)

import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Player, Vector, vectorIntToFloat, IntVector)


playerSpriteSize : IntVector
playerSpriteSize =
    ( 128, 128 )


playerHitBoxSize : IntVector
playerHitBoxSize =
    ( 64, 64 )


getPlayerLeftKickPoint : Vector -> Vector
getPlayerLeftKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            getX playerSpriteSize - getX playerHitBoxSize

        spriteBoxLeftSide =
            x - toFloat spriteHitBoxSizeDif / 2

        kickPointY =
            ((toFloat <| getY playerHitBoxSize) / 2) + y
    in
        ( spriteBoxLeftSide, kickPointY )


getPlayerRightKickPoint : Vector -> Vector
getPlayerRightKickPoint ( x, y ) =
    let
        spriteHitBoxSizeDif =
            getX playerSpriteSize - getX playerHitBoxSize

        spriteBoxRightSide =
            toFloat spriteHitBoxSizeDif / 2 + (toFloat <| getX playerHitBoxSize) + x

        kickPointY =
            ((toFloat <| getY playerHitBoxSize) / 2) + y
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
                , size = vectorIntToFloat playerHitBoxSize
                }

        fullSprite =
            Render.sprite
                { texture = Resources.getTexture "./assets/player-background-glow.png" resources
                , position = ( x - 32, y - 32 )
                , size = vectorIntToFloat playerSpriteSize
                }
    in
        [ fullSprite
        , hitBox
        ]
