module Linear where

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)

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
  let dt = map (\i -> fromIntegral i / fromIntegral h) [0..h]
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

simulatedMeanExitTime :: Double -> Double -> Double -> Double -> Int -> Double -> Double -> Int -> Double
-- x0 : X(0)
-- t : end time
-- mu, sigma : μ, σ
-- h : number of steps
-- a, b : interval 
-- sampleSize : number of sample paths
simulatedMeanExitTime x0 t mu sigma h a b sampleSize =
  let paths = map (\seed -> sample' (mkStdGen seed) (xt x0 t mu sigma h)) [0 .. sampleSize]
      exitTime path =
        case filter (\(_, x) -> x <= a || x >= b) (zip [0.0 .. fromIntegral h] path) of
          [] -> t  -- no exit
          ((time, _):_) -> time
  in sum (map (/ fromIntegral h) $ map exitTime paths) / fromIntegral sampleSize

main :: IO ()
main = do
  let a = 1.0; b = 2.0; mu = 1.0; sigma = 0.6
      xs = [1, 1.005 .. 2]; h = 200
      analytical = map (\x -> meanExitTime x a b mu sigma) xs
      numerical = map (\x0 -> simulatedMeanExitTime x0 1.0 mu sigma h a b 100) xs

      key = [("analytical", Just "blue"), ("numerical", Just "yellow")]
      legend = makeLegend key

  putStrLn ("\x1b[65C Mean exit time of X(t) on the interval (" 
    ++ show a ++ ", " ++ show b ++ "), with μ = " ++ show mu ++ ", σ = " ++ show sigma)
  putStr legend
  plotWith options { colors = map snd key } [analytical, numerical]

