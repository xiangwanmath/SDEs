module Linear where

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)

-- Plot solution to dX(t) = μ * X(t)dt + σ * X(t)dW(t)
-- X(t) = X(0) * e^[(μ - 1/2 * σ^2)t + σ * W(t)]

xt :: Double -> Double -> Double -> Double -> Int -> DRandom [Double]
-- x0 : X(0)
-- t : end time
-- mu, sigma : μ, σ
-- h : number of steps
xt x0 t mu sigma h = do 
  wt <- wiener t h
  let dt = map (\i -> fromIntegral i / fromIntegral h) [0..h]
      path = zipWith (\t w -> x0 * exp ((mu - 0.5 * sigma * sigma) * t + sigma * w)) dt wt
  return path

main :: IO ()
main = do
  g <- newStdGen
  let sigmas = [(0.2, Just "blue"), (0.4, Just "green"), (0.6, Just "magenta"), (0.8, Just "red")]

  mapM_ (\sigma -> do
    let solution = sample' g (xt 1.0 1.0 1.0 (fst sigma) 200)
    putStrLn ("Sample path X(t) on the interval [0,1], with X(0) = μ = 1, σ = " ++ show (fst sigma))
    plotWith options {color = (snd sigma)} solution) sigmas

  mapM_ (\sigma -> do
    let meanSolution = averagePath $ take 10 $ sample g (xt 1.0 1.0 1.0 (fst sigma) 200)
    putStrLn ("Average of 10 sample paths X(t) on the interval [0,1], with X(0) = μ = 1, σ = " ++ show (fst sigma))
    plotWith options {color = (snd sigma)} meanSolution) sigmas
