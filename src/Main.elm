module Main exposing (..)

import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { bird : Bird }


init =
    ({ bird = Bird.init }, Cmd.none)



update model =
    ( model, Cmd.none )



subscriptions =
    Sub.none


view model =
    div [] [
        model.bird.view
    ]
