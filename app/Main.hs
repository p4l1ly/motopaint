{-# LANGUAGE TupleSections #-}

module Main where

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

generate :: State StdGen [[(Double, Double)]]
generate = do
  motors <- replicateM count$ generateMotors 2 300 400
  let motors' = map (simpleKinematics speed0 distance angle) motors

  p0s <- replicateM count$
    (,) <$> (state$ randomR (0, 1000)) <*> (state$ randomR (0, 3))
    :: State StdGen [(Double, Double)]

  return$ (map.map) fst$ zipWith trajectory (map (, 0) p0s) motors'

  where
    count = 100
    distance [a, b] = 0.01 * (a + b) / 2
    angle [a, b] = 0.01 * (a - b)
    speed0 = 10

main = do
  lines <- getStdRandom$ runState generate
  return ()
