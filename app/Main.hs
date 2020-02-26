{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.

import Control.Arrow
import Control.Monad.State
import Data.List
import System.Random


type Accel = [Double] -> Double

simpleKinematics :: Double -> Accel -> Accel -> [[Double]] -> [(Double, Double)]
simpleKinematics speed0 distance angle ms = (`zip` map angle ms) $
  map (speed0 +) . tail $ scanl (+) 0 $ map distance ms

trajectory :: ((Double, Double), Double) -> [(Double, Double)]
           -> [((Double, Double), Double)]
trajectory = scanl $ \((x, y), o) (s, a) -> let o' = o + a / 2
  in ((x + s * cos o', y + s * sin o'), o + a)

generateMotors :: Int -> Int -> Int -> State StdGen [[Double]]
generateMotors motors from to = do
  iters <- state $ randomR (from, to)
  replicateM iters . replicateM motors . state $ randomR (-1, 1)

generateStem :: State StdGen [[(Double, Double)]]
generateStem = do
  motors <- replicateM count$ generateMotors 2 100 130
  let motors' = map (simpleKinematics speed0 distance angle) motors

  p0s <- replicateM count$
    (,) <$> (state$ randomR (0, 10)) <*> (state$ randomR (0, 2))
  let p0s' = zipWith (\(x, y) xo -> (x + xo, y)) p0s [0, maxx / count .. maxx]

  return$ (map.map) fst$ zipWith trajectory (map (, 0) p0s') motors'

  where
    maxx = 1000
    count = 30
    distance [a, b] = 0.01 * (a + b) / 2
    angle [a, b] = 0.001 * (a - b)
    speed0 = 0.7

draw :: [[(Double, Double)]] -> Double -> Diagram B
draw lines w = mconcat$ concat$ flip map lines$ \line ->
  zipWith3
    (\start end w ->
      fromVertices [p2 start, p2 end] # lwO w # lcA (black `withOpacity` 0.5)
    )
    line (tail line) $ map (*w)$
      startw ++ replicate (length line - 1 - 2*startwlen) 1 ++ endw
  where
    startw = [0.01, 0.1 .. 1]
    endw = reverse startw
    startwlen = length startw

screwYs =
  [ 562.6
  , 385.6
  , 209.5
  , 33.5
  ]

lineYs = concatMap (\y -> [y - treeW, y + treeW]) screwYs

treeW = 10

generateCrown :: (Double, Double) -> Double -> State StdGen [[(Double, Double)]]
generateCrown (angle1, angle2) angleDiff = do
  motors <- replicateM count$ generateMotors 2 300 300
  let motors' = map (simpleKinematics speed0 distance angle) motors

  p0s <- replicateM count$ (\a b c -> ((a, b), c))
    <$> (state$ randomR (0, 400))
    <*> (state$ randomR (-treeW, treeW))
    <*> (state$ randomR (pi + angle1, pi + angle2))

  return$ (map.map) fst$ zipWith trajectory p0s motors'

  where
    count = 80
    distance [a, b] = 0.05 * (a + b) / 2
    angle [a, b] = angleDiff + 0.03 * (a - b)
    speed0 = 0.3

crownAngles =
  [ ((-pi/4, pi/4), 0.003)
  , ((-pi/4, pi/4), 0)
  , ((-pi/4, pi/4), 0)
  , ((-pi/4, pi/4), -0.003)
  ]

main = do
  stems <- getStdRandom$ runState$ replicateM (length lineYs) generateStem
  crowns <- getStdRandom$ runState$ mapM (uncurry generateCrown) crownAngles
  mainWith$ mconcat$
    zipWith (\line y -> draw line 1 # translateY y) stems lineYs
    ++ zipWith (\line y -> draw line 2 # translateY y) crowns screwYs
