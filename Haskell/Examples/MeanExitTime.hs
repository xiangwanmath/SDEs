{-# LANGUAGE ExtendedDefaultRules #-}

module MeanExitTime where

import DRandom
import Chart

import Data.List (nub, maximumBy, find)
import Data.Ord (comparing)
import System.IO
import System.Random hiding (uniform, sample, random, next)
import Graphics.Matplotlib as Mt

-- Helper; takeWhile that includes the last value
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x ys -> if p x then x:ys else [x]) []

-- Given SDE dX(t) = μ * X(t)ⁿ dt + σ * X(t)ᵐ dW(t) and initial value X(0) = X_0 ϵ (a,b),
--   define T_{exit} as the first time that solution leaves the open interval (a,b)

-- Assuming X_0 = x, consider E[T_{exit}(x)] = u(x), then u(x) satisfies the BVP:
--   (μ/γ) * x²ⁿ u'' + μ * xᵐ u' = -1 where = (2μ/σ²)
-- for x >> 1, x ϵ (a,b), u(a) = u(b) = 0.

-- Parameters 
method = eulerMaruyama'

mu = 0.6 
sigma = 0.5
m = 1
n = 1

f x = mu * x^m
g x = sigma * x^n
(a, b) = (2.0, 5.0)

timeInterval = 100
nSteps = 1000
dt = timeInterval / nSteps
mesh = [0.0, 0.0 + dt .. timeInterval]

nICs = 150
dx0 = (b - a) / fromIntegral nICs
xs = [a, a + dx0 .. b]

nTrials = 1000

-- Generate the solution path of an SDE until it exits a certain interval (a, b) 
solveUntilExit method f g mesh (a, b) x0 seed = zip mesh results
  where 
    results = takeWhile' (\x -> x > a && x < b) $ scanl (\x (seed, (t1, t2)) -> method seed f g x (t1, t2)) x0 gens
    intervals = zip mesh (tail mesh)
    gens = zip (nGens (length intervals) (mkStdGen seed)) intervals

exitTime method f g mesh (a, b) x0 seed = fst $ last $ solveUntilExit method f g mesh (a, b) x0 seed

numericalMET = zipWith (avgExitTime) xs icSeeds
  where 
    icSeeds = take (nICs + 1) $ nub $ randomRs (0, 10000) (mkStdGen 730)
    trialSeeds s = take nTrials $ nub $ randomRs (0, 10000) (mkStdGen s)
    avgExitTime x0 seed = average $ map (exitTime method f g mesh (a, b) x0) (trialSeeds seed)

a1 :: Floating a => a -> a -> a -> a -> a -> a
-- a, b : interval 
-- mu, sigma : μ, σ
a1 a b mu sigma x = -- if (mu == 1 && sigma == 1) then -(log x)^2 + ((log a) + (log b))*(log x) - (log a)*(log b) else 
  (1 / ((0.5 * sigma * sigma) - mu)) *
  (log (x / a) -
   ((1 - ((x / a) ** (1 - (2 * mu / (sigma * sigma))))) /
    (1 - ((b / a) ** (1 - (2 * mu / (sigma * sigma)))))) *
   log (b / a))

analyticalMET = map (a1 a b mu sigma) xs

errorAtMax :: [Double] -> [Double] -> Double
errorAtMax analyticalMET numericalMET =
    abs $ (snd analyticalMax) - (snd numericalAtMax)
  where 
    analyticalMax = maximumBy (comparing snd) $ zip xs analyticalMET
    numericalAtMax = case find (\(x', _) -> x' == fst analyticalMax) pairs of
        Just pair -> pair
        Nothing   -> error "Invalid comparison"
    pairs = zip xs numericalMET
    

plotMeanExitTime :: IO ()
plotMeanExitTime = onscreen $ Mt.plot xs analyticalMET @@ [o2 "label" "analytical"]
  % Mt.plot xs (numericalMET) @@ [o2 "label" "numerical"]
  % title ("Mean exit time of X(t) on the interval (" 
    ++ show a ++ ", " ++ show b ++ ")") @@ [o2 "fontsize" 11]
  % legend @@ [o2 "loc" "upper right"]
