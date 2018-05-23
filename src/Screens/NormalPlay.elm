module Screens.NormalPlay
    exposing
        ( NormalPlayState
        , initialNormalPlayState
        , renderNormalPlay
        , LevelData
        , createLevel
        , updateNormalPlay
        , jsonToLevelData
        )

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import Enemy exposing (Enemy)
import GamePlatform exposing (Platform, renderPlatform, platformWithLocationsDecoder, platformSize)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Dict exposing (Dict)
import Coordinates exposing (gameSize, pixelToGridConversion, gridToPixelConversion, locationToGridCoordinate)
import Color
import GameTypes
    exposing
        ( Vector
        , IntVector
        , Player
        , TempProperties
        , vectorFloatToInt
        , vectorIntToFloat
        )
import Controller
    exposing
        ( Controller
        , ButtonState
            ( Pressed
            , Held
            , Released
            , Inactive
            )
        , DPadHorizontal(DPadRight, DPadLeft, NoHorizontalDPad)
        , DPadVertical(DPadUp, DPadDown, NoVerticalDPad)
        )
import Player
    exposing
        ( renderPlayer
        , initialPlayer
        , calculateAction
        , actionUpdate
        , updateRoutineX
        , collisionX
        , updateRoutineY
        , collisionY
        )


type alias NormalPlayState =
    { player : Player
    , permanentEnemies : List Enemy
    , enemies : List Enemy
    , platforms : Dict IntVector Platform
    , camera : Camera
    , resources : Resources
    , paused : Bool
    }



------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------


initialNormalPlayState : NormalPlayState
initialNormalPlayState =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = initialPlayer
        , permanentEnemies = []
        , enemies = []
        , platforms = Dict.empty
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , paused = False
        }


createLevel : LevelData -> NormalPlayState
createLevel levelData =
    let
        startingPoint =
            ( -300, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = initialPlayer
        , platforms = levelData.platforms
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , permanentEnemies = []
        , enemies = []
        , paused = False
        }


type alias LevelData =
    { platforms : Dict IntVector Platform
    }



--------------------------------------------------------------------------------
------------------------temporary functions ------------------------------------
--------------------------------------------------------------------------------
-- These are here for play testing and deciding what feels best. Once the game feel has
-- been decided these will be removed and replaced by constants


calculateYGravityFromJumpProperties : Float -> Float -> Float
calculateYGravityFromJumpProperties maxJumpHeight framesToApex =
    (2 * maxJumpHeight) / (framesToApex * framesToApex)


calculateInitialJumpVelocityFromJumpProperties : Float -> Float -> Float
calculateInitialJumpVelocityFromJumpProperties maxJumpHeight gravity =
    sqrt <| abs (2 * gravity * maxJumpHeight)


calculateEarlyJumpTerminationVelocity : Float -> Float -> Float -> Float -> Float
calculateEarlyJumpTerminationVelocity initialJumpVel gravity maxJumpHeight minJumpHeight =
    sqrt <| abs ((initialJumpVel * initialJumpVel) + (2 * gravity * (maxJumpHeight - minJumpHeight)))


calculateFriction : Float -> Float -> Float
calculateFriction acceleration maxSpeed =
    -((acceleration - maxSpeed) / maxSpeed)



------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
---------------------------Helper functions-------------------------------------
--------------------------------------------------------------------------------


capPlayerVelocity : Float -> Float -> Float
capPlayerVelocity topSpeed velocity =
    clamp -topSpeed topSpeed velocity



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


updateNormalPlay : Controller -> NormalPlayState -> TempProperties -> NormalPlayState
updateNormalPlay controller state tempProperties =
    let
        { player, platforms } =
            state

        newPlayer =
            calculateAction tempProperties controller platforms player
                |> actionUpdate tempProperties player
                |> updateRoutineX tempProperties controller
                |> collisionX platforms
                |> updateRoutineY tempProperties controller
                |> collisionY platforms
    in
        { state | player = newPlayer }


renderNormalPlay : NormalPlayState -> ( Int, Int ) -> List Renderable
renderNormalPlay state ( dash, recover ) =
    List.concat
        [ (List.map (\( gridCoordinate, platform ) -> renderPlatform Color.grey gridCoordinate) (Dict.toList state.platforms))
        , renderPlayer state.resources state.player ( dash, recover )
        ]


jsonToLevelData : Json.Decode.Value -> Result String LevelData
jsonToLevelData levelDataJson =
    Json.Decode.decodeValue levelDataDecoder levelDataJson


levelDataDecoder : Decoder LevelData
levelDataDecoder =
    let
        platforms =
            Json.Decode.list platformWithLocationsDecoder
                |> Json.Decode.map Dict.fromList
    in
        decode LevelData
            |> required "platforms" platforms
