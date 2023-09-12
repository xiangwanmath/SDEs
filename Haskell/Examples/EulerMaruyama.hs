{-# LANGUAGE ExtendedDefaultRules #-}

module EulerMaruyama where

import DRandom
import Chart

import Graphics.Matplotlib as Mt
import System.Random hiding (uniform, sample, random, next)
import Data.List (transpose)
import System.IO

-- For the SDE: dX(t) = f[X(t)]dt + g[X(t)]dW(t)
-- An approximate solution X_n ≈ X(t_n) is given by 
--          X_{n+1} = X_n + Δt * f(X_n) + ΔW_n * g(X_n) with ΔW_n = W(t_{n+1}) - W(t_n)

f :: Double -> Double
f x = x^3

g :: Double -> Double
g x = 0.6

eulerMaruyama :: Double -> Double -> (Double -> Double) -> (Double -> Double) -> Int -> DRandom [Double]
-- x0 : X(0)
-- t : end time
-- f, g
-- h : number of steps
eulerMaruyama x0 t f g h = do 
  wt <- wiener t h
  let dt = t / fromIntegral h
      path = scanl (\xn i -> xn + dt * f (xn) + (wt !! (i+1) - wt !! i) * g (xn)) x0 [0 .. (h-1)]
  return path

-- Analytical solution for f = μ, g = σ
xt :: Double -> Double -> (Double -> Double) -> (Double -> Double) -> Int -> DRandom [Double]
-- x0 : X(0)
-- t : end time
-- mu, sigma : μ, σ
-- h : number of steps
xt x0 t mu sigma h = do 
  wt <- wiener t h
  let dt = [ 0*t/(fromIntegral h), 1*t/(fromIntegral h) .. t]
      path = zipWith (\t w -> x0 * exp (( (mu t) - 0.5 * (sigma t) * (sigma t)) * t + (sigma t) * w)) dt wt
  return path

-- Strong error e^{strong} = sup_{0 ≤ t_n ≤ T} E[|(Y_n) - (X(t_n))|]
strongError :: (Ord a, Fractional a) =>
    Int -> DRandom [a] -> DRandom [a] -> DRandom a
-- xt : analytical solution
-- yt : approximated solution
-- sampleSize : number of sample paths
strongError sampleSize xt yt = do
  let xPaths = map (\seed -> sample' (mkStdGen seed) xt) [0 .. sampleSize]
      yPaths = map (\seed -> sample' (mkStdGen seed) yt) [0 .. sampleSize]
  return $ maximum $ averagePath $ [[ abs (y - x)| (x, y) <- zip xs ys] | (xs, ys) <- zip xPaths yPaths ]

-- Weak error e^{weak} = sup_{0 ≤ t_n ≤ T} |E[Y_n] - E[X(t_n)]|
weakError :: (Ord a, Fractional a, Foldable t1, Foldable t2) => 
    Int -> DRandom (t1 a) -> DRandom (t2 a) -> DRandom a
-- xt : analytical solution
-- yt : approximated solution
-- sampleSize : number of sample paths
weakError sampleSize xt yt = do
  let xPaths = map (\seed -> sample' (mkStdGen seed) xt) [0 .. sampleSize]
      yPaths = map (\seed -> sample' (mkStdGen seed) yt) [0 .. sampleSize]
  return $ maximum $ zipWith (\x y -> abs (y - x)) (map average xPaths) (map average yPaths)

plotEM :: IO ()
plotEM = onscreen $ Mt.plot ts em @@ [o2 "label" "X(t)"]
  % title ("Approximate solution X(t), with f = 1, g = 0.6") @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "lower right"]
  where
    x0 = 1.0; t = 1; h = 500
    ts = [ 0*t/(fromIntegral h), 1*t/(fromIntegral h) .. t]

    em = sample' (mkStdGen 30) (eulerMaruyama x0 t f g h)

emError :: IO ()
emError = do
  let x0 = 1; t = 1
      hs = [200 .. 700]; sampleSize = 10
      xts = map (\h -> xt x0 t f g h) hs
      yts = map (\h -> eulerMaruyama x0 t f g h) hs
      weakErr = zipWith (weakError sampleSize $) xts yts
      strongErr = zipWith (strongError sampleSize $) xts yts
  writeFile "Examples/data/emError.txt" $ 
      "Sample Size: " ++ (show sampleSize) ++
      "\n\nh Range: " ++ (show $ head hs) ++ "-" ++ (show $ last hs) ++
      "\n\nWEAK ERROR: \n" ++ (show weakErr) ++ 
      "\n\nSTRONG ERROR: \n" ++ (show strongErr)
