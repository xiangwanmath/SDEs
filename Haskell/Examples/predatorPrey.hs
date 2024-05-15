{-# LANGUAGE ExtendedDefaultRules #-}

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)
import Graphics.Matplotlib as Mt

r = 0.4           -- growth rate of x
k = 1.3           -- environment capacity
lambda = 1.3      -- excess prey ingested   
b = 0.25          -- searching rate
h = 0.5           -- handling time
d1 = 0.2          -- natural death of y
sigma1 = 0        -- random perturbation on x
sigma2 = 0        -- random perturbation on y
a = 0.1

x0 = 0.9
y0 = 0.8
t = 1000
nSteps = 10000
dt = fromIntegral t / fromIntegral nSteps

sampleSize = 10

wt1s = map (\seed -> take nSteps $ sample (mkStdGen seed) (normal 0 1)) [101..sampleSize+101]
wt2s = map (\seed -> take nSteps $ sample (mkStdGen seed) (normal 0 1)) [sampleSize+433..2*sampleSize+434]

-- samplePath (x0, y0) wt1 wt2 = (map fst paths, map snd paths)
--   where paths = scanl (\(xn, yn) i ->
--                   (xn + xn * (r - (r * xn / k) - b * yn) * dt + sigma1 * xn * (sqrt dt) * (wt1 !! i) + ((sigma1^2 * xn) / 2) * ((wt1 !! i)^2 - 1) * dt
--                   , yn + yn * ((lambda * b * xn) / (1 + b * h * xn) - d1) * dt + sigma2 * yn * (sqrt dt) * (wt2 !! i) + ((sigma2^2 * yn) / 2) * ((wt2 !! i)^2 - 1) * dt
--                   ))
--                   (x0, y0) [0 .. (nSteps-1)]

samplePath (x0, y0) wt1 wt2 = (map fst paths, map snd paths)
  where paths = scanl (\(xn, yn) i ->
                  (xn + xn * (xn - a) * (1 - xn / k) - (b * yn * xn) * dt + sigma1 * xn * (sqrt dt) * (wt1 !! i) + ((sigma1^2 * xn) / 2) * ((wt1 !! i)^2 - 1) * dt
                  , yn + yn * ((lambda * b * xn) / (1 + b * h * xn) - d1) * dt + sigma2 * yn * (sqrt dt) * (wt2 !! i) + ((sigma2^2 * yn) / 2) * ((wt2 !! i)^2 - 1) * dt
                  ))
                  (x0, y0) [0 .. (nSteps-1)]

paths = zipWith (\wt1 wt2 -> samplePath (x0, y0) wt1 wt2) wt1s wt2s
(meanX, meanY) = (averagePath $ map fst paths, averagePath $ map snd paths)

xt = tail $ zipWith (\x i -> log (x) / (dt * fromIntegral i)) meanX [0 .. length meanX]

main :: IO ()
main = do
  let key = [("x(t)", Just "green"), ("y(t)", Just "magenta")]
  writeFile "Examples/data/predatorPrey.txt" $ 
      "(r, b, h, λ, d₁, K, σ₁, σ₂): " ++ show (r, b, h, lambda, d1, k, sigma1, sigma2) ++ 
      "\n(x0, y0, t, number of steps): " ++ show (x0, y0, t, nSteps) ++
      "\nx(t): " ++ (show meanX) ++
      "\ny(t): " ++ (show $ meanY)
  
  if (nSteps <= 230) 
    then do 
      putStrLn $ makeLegend key
      plotWith options { colors = map snd key } [meanX, meanY]
    else do putStrLn "Data saved to data/predatorPrey.txt"
  putStrLn $ makeLegend key
  plotWith options { colors = map snd key } [meanX, meanY]

plotMeans :: IO ()
plotMeans = onscreen $ Mt.plot meanX meanY
  -- % Mt.plot [0, 0+dt .. fromIntegral t] meanY @@ [o2 "label" "y(t)"]
  % title ("Mean of " ++ show sampleSize ++ " paths, with (r, b, h, λ, d₁, K, σ₁, σ₂): " ++ show (r, b, h, lambda, d1, k, sigma1, sigma2) ++ " and " ++ show nSteps ++ " steps") @@ [o2 "fontsize" 11]
  -- % legend @@ [o2 "loc" "upper right"]

