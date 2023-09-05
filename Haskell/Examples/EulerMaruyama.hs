{-# LANGUAGE ExtendedDefaultRules #-}

module EulerMaruyama where

import DRandom
import Chart

import Graphics.Matplotlib as Mt
import System.Random hiding (uniform, sample, random, next)
import Data.List (transpose)


-- For the SDE: dX(t) = f[X(t)]dt + g[X(t)]dW(t)
-- An approximate solution X_n ≈ X(t_n) is given by 
--          X_{n+1} = X_n + Δt * f(X_n) + ΔW_n * g(X_n) with ΔW_n = W(t_{n+1}) - W(t_n)

f :: Double -> Double
f x = 1

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


plotEM :: IO ()
plotEM = onscreen $ Mt.plot ts em @@ [o2 "label" "X(t)"]
  % title ("Approximate solution X(t), with f = 1, g = 0.6") @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "lower right"]
  where
    x0 = 1.0; t = 1; h = 500
    ts = [ 0*t/(fromIntegral h), 1*t/(fromIntegral h) .. t]

    em = sample' (mkStdGen 30) (eulerMaruyama x0 t f g h)
