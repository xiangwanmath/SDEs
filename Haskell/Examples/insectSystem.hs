{-# LANGUAGE ExtendedDefaultRules #-}

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)
import Graphics.Matplotlib as Mt

r = 0.5
c = 10.0
a = 1.0
b = 1.0
tau = 1.0
sigma = 0.4

x0 = 0.5
y0 = 0.1
t = 1000
h = 100000

sampleSize = 1000

--em2 :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> DRandom [[Double]]
em2 x0 y0 r c a b tau sigma h = do
  wt <- wiener t h
  let dt = t / fromIntegral h
      fX x y = r * x * (1 - x / c) - (b * x * x) / (a * a + x * x) + x * y
      fY y = (-1 / tau) * y
      gY _ = (1 / tau) * (sigma / sqrt 2) -- g(Y) doesn't depend on Y
      pathY = scanl (\yn i -> yn + dt * fY yn + (wt !! (i+1) - wt !! i) * gY yn) y0 [0 .. (h-1)]
      pathX = scanl (\xn i -> xn + dt * fX xn (pathY !! i)) x0 [0 .. (h-1)]
  return [pathX, pathY]

main :: IO ()
main = do
  let paths = map (\seed -> sample' (mkStdGen seed) (em2 x0 y0 r c a b tau sigma h)) [1 .. sampleSize]
      meanX = averagePath $ map head paths
      meanY = averagePath $ map (head . tail) paths
      key = [("X(t)", Just "green"), ("Y(t)", Just "magenta")]
  writeFile "Examples/data/insectSystem.txt" $ 
      "(r, C, a, b, τ, σ): " ++ show (r, c, a, b, tau, sigma) ++ 
      "\n(x0, y0, t, h): " ++ show (x0, y0, t, h) ++
      "\nX(t): " ++ (show meanX) ++
      "\nY(t): " ++ (show $ meanY)
  if (h <= 230) 
    then do 
      putStrLn $ makeLegend key
      plotWith options { colors = map snd key } [meanX, meanY]
    else do putStrLn "Data saved to data/insectSystem.txt"


plotMeans :: IO ()
plotMeans = onscreen $ Mt.plot [0, 0+dt .. t] meanX @@ [o2 "label" "X(t)"]
  % Mt.plot [0, 0+dt .. t] meanY @@ [o2 "label" "Y(t)"]
  % title ("Mean of " ++ show sampleSize ++ " paths, with r = " ++ show r ++ ", C = " ++ show c ++ ", a = " ++ show a ++ ", b = " ++ show b ++ ", τ = " ++ show tau ++ ", σ = " ++ show sigma ++ ", and h = " ++ show h) @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "lower right"]
  where
    dt = t / fromIntegral h
    paths = map (\seed -> sample' (mkStdGen seed) (em2 x0 y0 r c a b tau sigma h)) [1 .. sampleSize]
    meanX = averagePath $ map head paths
    meanY = averagePath $ map (head . tail) paths
