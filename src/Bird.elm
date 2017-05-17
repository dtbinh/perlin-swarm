module Bird exposing (Bird, init, update, view)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Vector2 exposing (..)
import Vector3 exposing (..)
import Perlin


type alias Bird =
    { position : Float2
    , heading : Float2
    , speed : Float
    , leader : Bool
    }


init : Float -> Float -> Bool -> Bird
init x y leading =
    { position = ( x, y )
    , heading = ( 1, 0 )
    , speed = 0.1
    , leader = leading
    }


update : Time -> Int -> Int -> Perlin.Noise -> List Bird -> Bird -> Bird
update time width height ether birds bird =
    let
        filteredBirds =
            List.filter (\b -> b /= bird) birds

        leaders =
            List.filter (\b -> b.leader) filteredBirds

        closestLeader =
            if bird.leader then
                Nothing

            else
                case List.head leaders of
                    Nothing ->
                        Nothing

                    Just b ->
                        findClosestLeader b bird leaders

        closestBird =
            case List.head filteredBirds of
                Nothing ->
                    Nothing

                Just b ->
                    findClosestBird b bird filteredBirds

        newHeading =
            case closestLeader of
                Nothing ->
                    -- Vector2.directionFromTo bird.position ( (toFloat targetX), (toFloat targetY) )
                    -- ((Noise.noise3d ether (Vector2.getX bird.position) (Vector2.getY bird.position) time) , 1)
                    -- rotateVector (1,0) (pi * (Noise.noise2d ether (Vector2.getX bird.position) (Vector2.getY bird.position)))
                    -- rotateVector (1,0) (-0.5 * pi)
                    rotateVector (1,0) (4 * pi * (ether ((Vector2.getX bird.position)/4, (Vector2.getY bird.position)/4)))

                Just leader ->
                    case closestBird of
                        Nothing ->
                            Vector2.directionFromTo bird.position leader.position

                        Just cb ->
                            if Vector2.distance bird.position cb.position > 30
                            then
                                Vector2.directionFromTo bird.position leader.position
                            else
                                -- Vector2.sub bird.heading cb.heading
                                -- rotateVector bird.heading ((Vector2.angle bird.heading cb.heading)/2)
                                rotateVector bird.heading 0.8

        currentSpeed =
            if bird.leader
            then
                bird.speed + 0.05
            else
                bird.speed
            -- case closestBird of
            --     Nothing ->
            --         bird.speed
            --
            --     Just cb ->
            --         if Vector2.distance bird.position cb.position > 20
            --         then
            --             bird.speed
            --         else
            --             bird.speed * (Vector2.distance bird.position cb.position)/20

        ( x, y ) =
            Vector2.scale currentSpeed newHeading
                |> Vector2.scale time
                |> Vector2.add bird.position

        adjustedPosition =
            ( adjustToBoundary x (toFloat width), adjustToBoundary y (toFloat height) )
    in
        { bird | position = adjustedPosition, heading = newHeading }


findClosestBird : Bird -> Bird -> List Bird -> Maybe Bird
findClosestBird closest bird birds =
    case birds of
        [] ->
            Just closest

        h :: t ->
            if (Vector2.distance bird.position closest.position) > (Vector2.distance bird.position h.position)
              && Vector2.angle bird.heading (Vector2.sub h.position bird.position) < (degrees 45)
              -- && h.leader
            then
                findClosestBird h bird t
            else
                findClosestBird closest bird t

findClosestLeader : Bird -> Bird -> List Bird -> Maybe Bird
findClosestLeader closest bird birds =
    case birds of
        [] ->
            Just closest

        h :: t ->
            if (Vector2.distance bird.position closest.position) > (Vector2.distance bird.position h.position)
              -- && Vector2.angle bird.heading (Vector2.sub h.position bird.position) < (degrees 45)
              && h.leader then
                findClosestBird h bird t
            else
                findClosestBird closest bird t


adjustToBoundary : Float -> Float -> Float
adjustToBoundary scalar boundary =
    if scalar > half boundary then
        scalar - boundary
    else if scalar < -(half boundary) then
        scalar + boundary
    else
        scalar


view : Bird -> List Form
view bird =
    let
        ( x, y ) =
            bird.position

        angle =
            Vector2.angle (1,0) bird.heading

        (headingX, headingY) =
            bird.heading

        (_, _, crossZ) =
            Vector3.cross (1, 0, 0) (headingX, headingY, 0)

        rotateAngle =
            if crossZ > 0
            then
                angle
            else
                2 * pi - angle

    in
        if bird.leader
        then
            []
        else
            [ (polygon [ ( -6, -4 ), ( 6, 0 ), ( -6, 4 ) ])
            -- [ (circle 12)
                |> outlined defaultLine
                |> rotate rotateAngle
                |> move ( x, y )
            ]


half : Float -> Float
half x =
    x / 2

rotateVector : Float2 -> Float -> Float2
rotateVector (vectorX, vectorY) angle =
    let
        x =
            vectorX * cos angle - vectorY * sin angle

        y =
            vectorX * sin angle + vectorY * cos angle

    in
        (x, y)
