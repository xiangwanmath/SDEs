module Logistic where

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)

-- Plot solution to dX(t) = r * X(t) * [K - X(t)]dt + β * X(t)dW(t)
-- X(t) = X(0) * e^[(rK - 1/2 * β^2)t + β * W(t)] / 
--        1 + X(0) * r * ∫₀ᵗ e^[(rK - 1/2 * β^2)s + β * W(s)]ds

xt :: Double -> Double -> Double -> Double -> Double -> Int -> DRandom [Double]
-- x0 : X(0)
-- t : end time
-- r, k, beta : r, K, β
-- h : number of steps
xt x0 t r k beta h = do
  wt <- wiener t h
  let dt = map (\i -> fromIntegral i / fromIntegral h) [0..h]
      exponential t w = exp((r * k - 0.5 * beta^2) * t + beta * w)
      integral t w = ito (0, t) $ exponential t w
      path = zipWith (\t w -> x0 * exponential t w / (1 + x0 * r * integral t w)) dt wt
  return path

main :: IO ()
main = do
  g <- newStdGen
  let betas = [(0.2, Just "blue"), (0.4, Just "green"), (0.6, Just "magenta"), (0.8, Just "red")]

  mapM_ (\beta -> do
    let solution = sample' g (xt 1.0 1.0 0.1 100.0 (fst beta) 200)
    putStrLn ("Sample path X(t) on the interval [0,1], with X(0) = μ = 1, σ = " ++ show (fst beta))
    plotWith options {color = (snd beta)} solution) betas

  mapM_ (\beta -> do
    let meanSolution = averagePath $ take 10 $ sample g (xt 1.0 1.0 0.1 100.0 (fst beta) 200)
    putStrLn ("Average of 10 sample paths X(t) on the interval [0,1], with X(0) = μ = 1, σ = " ++ show (fst beta))
    plotWith options {color = (snd beta)} meanSolution) betas