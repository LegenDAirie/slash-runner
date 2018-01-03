module Main exposing (main)

import App exposing (Model, Msg, view, init, update, subscriptions)
import Html exposing (program)


main : Program Never Model Msg
main =
    program { view = view, init = init, update = update, subscriptions = subscriptions }
