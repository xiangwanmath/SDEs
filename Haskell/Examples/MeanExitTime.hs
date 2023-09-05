{-# LANGUAGE ExtendedDefaultRules #-}

module MeanExitTime where

import DRandom
import Chart

import Data.List (transpose)
import System.Random hiding (uniform, sample, random, next)
import Graphics.Matplotlib as Mt

-- Given SDE dX(t) = μ * X(t)dt + σ * X(t)dW(t) and initial value X(0) = X_0 ϵ (a,b),
--   define T_{exit} as the first time that solution leaves the open interval (a,b)

-- Assuming X_0 = x, consider E[T_{exit}(x)] = u(x), then u(x) satisfies the BVP:
--   (1/2)g²(x) * d²u/dx² + f(x) * du/dx = -1 for x ϵ (a,b), u(a) = u(b) = 0.

-- Plot solution to BVP for dX(t) = μ * X(t)dt + σ * X(t)dW(t).
-- Plot numerical simulation of mean exit time.

xt :: Double -> Double -> Double -> Double -> Int -> DRandom [Double]
-- x0 : X(0)
-- t : end time
-- mu, sigma : μ, σ
-- h : number of steps
xt x0 t mu sigma h = do 
  wt <- wiener t h
  let dt = [ 0*t/(fromIntegral h), 1*t/(fromIntegral h) .. t]
      path = zipWith (\t w -> x0 * exp ((mu - 0.5 * sigma * sigma) * t + sigma * w)) dt wt
  return path

meanExitTime :: Floating a => a -> a -> a -> a -> a -> a
-- a, b : interval 
-- mu, sigma : μ, σ
meanExitTime x a b mu sigma =
  (1 / ((0.5 * sigma * sigma) - mu)) *
  (log (x / a) -
   ((1 - ((x / a) ** (1 - (2 * mu / (sigma * sigma))))) /
    (1 - ((b / a) ** (1 - (2 * mu / (sigma * sigma)))))) *
   log (b / a))


exitTime :: Double -> Double -> Double -> Double -> Int -> Double -> Double -> Int -> [Double]
-- x0 : X(0)
-- t : end time
-- mu, sigma : μ, σ
-- h : number of steps
-- a, b : interval 
-- sampleSize : number of sample paths
exitTime x0 t mu sigma h a b sampleSize =
  let paths = map (\seed -> sample' (mkStdGen seed) (xt x0 t mu sigma h)) [0 .. sampleSize]
      exitTime' path =
        case filter (\(_, x) -> x <= a || x >= b) (zip [0.0 .. fromIntegral h] path) of
          [] -> t  -- no exit
          ((time, _):_) -> time
  in (map (/ fromIntegral h) $ map exitTime' paths)

simulatedMeanExitTime :: Double -> Double -> Double -> Double -> Int -> Double -> Double -> Int -> Double
simulatedMeanExitTime x0 t mu sigma h a b sampleSize = sum (exitTime x0 t mu sigma h a b sampleSize) / fromIntegral sampleSize


plotMeanExitTime :: IO ()
plotMeanExitTime = onscreen $ Mt.plot xs analytical @@ [o2 "label" "analytical"]
  % Mt.plot xs numerical @@ [o2 "label" "numerical"]
  % title ("Mean exit time of X(t) on the interval (" 
    ++ show a ++ ", " ++ show b ++ "), with μ = " ++ show mu ++ ", σ = " ++ show sigma) @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "upper right"]
  where
    a = 2.0; b = 5.0; mu = 1.0; sigma = 0.6
    xs = [2, 1.005 .. 5]; h = 1000
    analytical = map (\x -> meanExitTime x a b mu sigma) xs
    numerical = map (\x0 -> simulatedMeanExitTime x0 1.0 mu sigma h a b 100) xs
 
plotError :: IO ()
plotError = onscreen $ Mt.plot hs weakErr @@ [o2 "label" "weak error"]
  % Mt.plot hs strongErr @@ [o2 "label" "strong error"]
  % title ("Error with μ = " ++ show mu ++ ", σ = " ++ show sigma) @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "upper right"]
  % xlabel "h"
  % ylabel "error"
  where
    a = 1.0; b = 2.0; mu = 1.0; sigma = 0.6
    xs = [1, 1.005 .. 2]; hs = [400 .. 700]; sampleSize = 10
    analytical = map (\x -> meanExitTime x a b mu sigma) xs
    err path = zipWith (\a n -> abs $ a - n) analytical path

    numericalMean = map (\h -> map (\x0 -> simulatedMeanExitTime x0 1.0 mu sigma h a b sampleSize) xs) hs
    weakErr = map (\ys -> maximum $ err ys) numericalMean
    
    averageList lists = map (\xs -> sum xs / fromIntegral (length xs)) (transpose lists)
    numerical = map (\h -> map (\x0 -> exitTime x0 1.0 mu sigma h a b sampleSize) xs) hs
    strongErr = map ( \ys -> maximum $ averageList (map (\sample -> err sample) ys) ) numerical
