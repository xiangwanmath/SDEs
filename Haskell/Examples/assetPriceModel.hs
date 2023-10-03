{-# LANGUAGE ExtendedDefaultRules #-}

import DRandom
import Chart

import System.Random hiding (uniform, sample, random, next)
import Data.List (transpose)
import Graphics.Matplotlib as Mt

alpha = 1.0

x0 = 1
y0 = 0.4
z0 = 0.4
t = 100
h = 200

sampleSize = 20

--system :: Double -> Double -> Double -> Double -> Int -> DRandom [[Double]]
system x0 y0 z0 alpha h = do
  wt1 <- wiener t h
  wt2 <- wiener t h
  let dt = t / fromIntegral h
      d wt i = (wt !! (i+1) - wt !! i)
      pathsYZ = scanl 
        (\(yn, zn) i -> (-(yn - zn) * dt + 0.3 * yn * d wt2 i, (1 / alpha) * (yn - zn) * dt)) 
        (y0, z0) [0 .. (h-1)]
      pathZ = map snd pathsYZ
      pathY = map fst pathsYZ
      pathX = scanl (\xn i -> xn * (pathY !! i) * (d wt1 i)) x0 [0 .. (h-1)]
  return [pathX, pathY, pathZ]

main :: IO ()
main = do
  let paths = map (\seed -> sample' (mkStdGen seed) (system x0 y0 z0 alpha h)) [1 .. sampleSize]
      means = map averagePath (transpose paths)
      key = [("X(t)", Just "green"), ("Y(t)", Just "magenta"), ("Z(t)", Just "blue")]
  if (h <= 230) 
    then do 
      putStrLn $ makeLegend key
      plotWith options { colors = map snd key } means
    else do putStrLn "Data saved to data/assetPriceModel.txt"
  writeFile "Examples/data/assetPriceModel.txt" $ 
      "α : " ++ show (alpha) ++ 
      "\n(x0, y0, z0, t, h): " ++ show (x0, y0, z0, t, h) ++
      "\nX(t): " ++ (show $ means !! 0) ++
      "\nY(t): " ++ (show $ means !! 1) ++
      "\nZ(t): " ++ (show $ means !! 2)

plotMeans :: IO ()
plotMeans = onscreen $ Mt.plot [0, 0+dt .. t] (means !! 0) @@ [o2 "label" "X(t)"]
  % Mt.plot [0, 0+dt .. t] (means !! 1) @@ [o2 "label" "Y(t)"]
  % Mt.plot [0, 0+dt .. t] (means !! 2) @@ [o2 "label" "Z(t)"]
  % title ("Mean of " ++ show sampleSize ++ " paths, with α = " ++ show alpha ++ ", h = " ++ show h) @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "upper right"]
  where
    dt = t / fromIntegral h
    paths = map (\seed -> sample' (mkStdGen seed) (system x0 y0 z0 alpha h)) [1 .. sampleSize]
    means = map averagePath (transpose paths)

