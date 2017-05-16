module Main exposing (..)

import Bird exposing (..)
import Html exposing (..)
import Collage exposing (..)
import Element exposing (..)
import AnimationFrame exposing (..)
import Random
import Mouse
import Random.Pcg as PCG
import Perlin
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick Float
    | NewBirdCoordinates ( Int, Int )
    | MouseMove Mouse.Position
    | TickTime Time


type alias Model =
    { birds : List Bird
    , width : Int
    , height : Int
    , mousePosition : Mouse.Position
    , ether : Perlin.Noise
    , time : Time
    }


initModel : Model
initModel =
    { birds = []
    , width = 1000
    , height = 800
    , mousePosition = Mouse.Position 0 0
    , ether = generateEther
    , time = 0
    }



-- initBirds : Int -> List Bird
-- initBirds amount =
--     List.repeat amount Bird.init


init : ( Model, Cmd Msg )
init =
    ( initModel, generateBirds initModel )


generateEther : Perlin.Noise
generateEther =
    Perlin.octaves 5 (PCG.initialSeed 227852860)

generateBirds : Model -> Cmd Msg
generateBirds model =
    -- initBirds 10
    Random.generate NewBirdCoordinates (Random.pair (Random.int -model.width model.width) (Random.int -model.height model.height))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let

                newBirds =
                    List.map (Bird.update time
                                          model.width
                                          model.height
                                          model.ether
                                          model.birds
                              )
                              model.birds
            in
                ( { model | birds = newBirds }, Cmd.none )

        TickTime currentTime ->
            ( { model | time = currentTime }, Cmd.none )


        NewBirdCoordinates ( x, y ) ->
            let
                newCmd =
                    if List.length model.birds < 100 then
                        generateBirds model
                    else
                        Cmd.none

                newBird =
                    Bird.init (toFloat x) (toFloat y) (List.length model.birds >= 96)
            in
                ( { model | birds = (newBird :: model.birds) }, newCmd )

        MouseMove position ->
            let
                newPosition =
                    Mouse.Position (position.x - (round ((toFloat model.width)/2))) (-1 * (position.y - (round ((toFloat model.height)/2))))

            in
                ( { model | mousePosition = newPosition }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AnimationFrame.diffs Tick
              , Mouse.moves MouseMove
              ]


view : Model -> Html Msg
view model =
    collage model.width model.height (List.concatMap Bird.view model.birds)
        |> container model.width model.height middle
        |> toHtml
