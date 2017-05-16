module Perlin exposing (Noise, noise, octaves)
{-|
Technically, this is an implementation of [Improved Noise](http://mrl.nyu.edu/~perlin/paper445.pdf), a refinement on the
original Perlin noise, but not Simplex Noise.
-}
import Random.Pcg as PCG
import Tuple

type alias Gradient = (Float, Float)

spiral : (Int, Int) -> Int
spiral (x,y) =
  let d = x+y
  in d*(d+1)//2 + x

lookup : PCG.Seed -> (Int, Int) -> Gradient
lookup seed pos =
  let seed1 = PCG.fastForward (spiral pos) seed
  in PCG.step generateGradient seed1 |> Tuple.first

generateGradient : PCG.Generator Gradient
generateGradient =
  PCG.map
    (\theta -> fromPolar (1, theta))
    (PCG.float 0 (2*pi))

dot : (Float, Float) -> (Float, Float) -> Float
dot (a, b) (c, d) = a*c + b*d

type alias Noise = (Float, Float) -> Float

noise : PCG.Seed -> Noise
noise seed (x,y) =
    let
      xi = floor x -- integer part of x
      xf = x - toFloat xi -- fractional/floating part of x
      yi = floor y
      yf = y - toFloat yi

      -- lookup four points on square around gradient
      gxx = lookup seed |> curry
      gbl = gxx  xi     yi -- gradient at bottom left
      gbr = gxx (xi+1)  yi -- gradient at bottom right, and so on
      gtl = gxx  xi    (yi+1)
      gtr = gxx (xi+1) (yi+1)

      -- get dot product of gradient points with distance to them (except maybe negative??)
      dbl = dot gbl (xf  , yf) -- dot bottom left
      dbr = dot gbr (xf-1, yf)
      dtl = dot gtl (xf  , yf-1)
      dtr = dot gtr (xf-1, yf-1)

      -- apply nonlinear interpolation
      fadeX = fade xf
      fadeY = fade yf
      nx1 = dbl*(1-fadeX) + dbr*fadeX
      nx2 = dtl*(1-fadeX) + dtr*fadeX
    in
      nx1*(1-fadeY) + nx2*fadeY + 0.4

-- nonlinear interpolation: 6t^5 - 15t^4 + 10t^3
fade : Float -> Float
fade t = t * t * t * (t * (t * 6 - 15) + 10)

octaves : Int -> PCG.Seed -> Noise
octaves n seed =
  let
    seeds = generateNSeeds n seed
    multipliers = List.map (\m -> 2^m) (List.map (\x -> toFloat x) (List.range 0 n))
    octave m seed (x,y) =
      noise seed (x/m, y/m) * m
    normalizer = List.sum multipliers
    noiseFuncs = List.map2 octave multipliers seeds
  in
    \pos -> List.sum (List.map (\f -> f pos) noiseFuncs) / normalizer

generateNSeeds : Int -> PCG.Seed -> List PCG.Seed
generateNSeeds n seed =
    let
        (xs, _) = PCG.step (PCG.list n <| PCG.independentSeed) seed

    in
        xs


-- generateNSeeds n seed =
--   let helper seeds =
--     if List.length seeds >= n then
--       List.take n seeds |> Debug.log "seeds"
--     else
--       List.concatMap (\seed -> let (x,y) = PCG.split seed in [x,y]) seeds |> helper
--   in
--     helper [seed]
