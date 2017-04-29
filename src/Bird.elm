module Bird exposing (Bird, init, update, view)


import Collage exposing (..)
import Color exposing (..)



type alias Bird =
    { x : Float
    , y : Float
    }


init =
    { x = 0
    , y = 0
    }


update bird =
    ( bird, Cmd.none )


view bird =
    (oval 15 15)
        |> filled white
        |> move ( bird.x, bird.y )
