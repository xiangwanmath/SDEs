module DRandom ( DRandom,
    random,
    uniformDiscrete,
    bernoulli,
    binomial,
    geometric,
    poisson,
    uniform,
    exponential,
    gamma,
    normal,
    multivariateNormal,
    wiener,
    eulerMaruyama,
    milstein,
    solveBy,
    eulerMaruyama',
    solveBy',
    brownianBridge,
    dFromIntegral,
    dFromBool,
    dSplitAt,
    nGens,
    average,
    sample,
    sample',
    sampleMean,
    sampleMeanVector,
    sampleVariance,
    averagePath,
    reduce,
    histogram,
  ) where

import System.Random hiding (uniform, sample, random)
import Control.Monad
import Data.List (unfoldr, transpose, nub)
import Numeric.LinearAlgebra

import Class
import Chart

-- Define the monad DRandom.

newtype DRandom a = DRandom { run :: StdGen -> a }

instance Functor DRandom where
  fmap f (DRandom action) = DRandom $ \gen ->
    let x = action gen
    in f x

instance Applicative DRandom where
  pure x = DRandom $ \gen -> x
  (DRandom f) <*> (DRandom x) = DRandom $ \gen ->
    let f' = f gen
        x' = x gen
    in f' x'

instance Monad DRandom where
  return = pure
  (DRandom x) >>= f = DRandom $ \gen ->
    let x' = x gen
        (DRandom y) = f x'
    in y gen

instance Num (DRandom Double) where
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  (-) = liftM2 (-)
  abs = liftM abs
  signum = liftM signum
  fromInteger = pure . fromInteger

instance Fractional (DRandom Double) where
  (/) = liftM2 (/)
  recip = liftM recip
  fromRational = pure . fromRational

instance Show (DRandom Double) where
  show (DRandom action) = show (action (mkStdGen 0))

-- Define DRandom as an instance of Distribution.

instance Distribution DRandom where

  random :: DRandom Double
  random = DRandom $ \gen -> fst (randomR (0.0, 1.0) gen)

  uniformDiscrete :: [a] -> DRandom a
  uniformDiscrete xs = DRandom $ \gen ->
    let (i, gen') = randomR (0, length xs - 1) gen
    in xs !! i

  bernoulli :: Double -> DRandom Bool
  bernoulli p = DRandom $ \gen ->
    let x = run random gen
    in x <= p

  binomial :: Int -> Double -> DRandom Int
  binomial n p = DRandom $ \gen ->
    let tosses = take n (sample gen random)
    in (length (filter (<= p) tosses))

  geometric :: Double -> DRandom Int
  geometric p = do
    u <- random
    return (ceiling (log (1 - u) / log (1 - p)))

  uniform :: Double -> Double -> DRandom Double
  uniform a b = do
    u <- random
    return (a + (b - a) * u)

  exponential :: Double -> DRandom Double
  exponential lambda = do
    u <- random
    return (- log (1 - u) / lambda)

  normal :: Double -> Double -> DRandom Double
  normal mu sigma = DRandom $ \gen ->
    let (u1, gen') = (run random gen, snd (split gen))
        u2 = run random gen'
    in (mu + sigma * sqrt (-2.0 * log u1) * cos (2.0 * pi * u2))

eulerMaruyama :: StdGen
     -> (Double -> Double)
     -> (Double -> Double)
     -> Double
     -> (Double, Double)
     -> (Double, StdGen)
eulerMaruyama gen f g x (t1, t2) = (x + dt * (f x) + dW * (g x), snd $ split gen)
  where dt = t2 - t1
        dW = run (normal 0 (sqrt dt)) gen

milstein :: StdGen
     -> (Double -> Double)
     -> (Double -> Double)
     -> Double
     -> (Double, Double)
     -> (Double, StdGen)
milstein gen f g x (t1, t2) = (x + dt * (f x) + dW * (g x) + 0.5 * (g x) * g' * ((dW^2) - dt), snd $ split gen)
  where dt = t2 - t1
        dW = run (normal 0 (sqrt dt)) gen
        g' = (g (x + dW) - g x) / dW

-- Solve general SDE: dX(t) = f[X(t)] dt + g[X(t)] dW(t)
solveBy method f g mesh x0 gen = map fst results
  where 
    results = scanl (\(x, gen') (t1, t2) -> method gen' f g x (t1, t2)) (x0, gen) intervals
    intervals = zip mesh (tail mesh)

eulerMaruyama' :: StdGen
     -> (Double -> Double)
     -> (Double -> Double)
     -> Double
     -> (Double, Double)
     -> Double
eulerMaruyama' gen f g x (t1, t2) = x + dt * (f x) + dW * (g x)
  where dt = t2 - t1
        dW = run (normal 0 (sqrt dt)) gen

-- Solve general SDE: dX(t) = f[X(t)] dt + g[X(t)] dW(t)
solveBy' method f g mesh x0 seed = results
  where 
    results = scanl (\x (gen, t) -> method gen f g x t) x0 gens
    intervals = zip mesh (tail mesh)
    gens = zip (nGens (length intervals) (mkStdGen seed)) intervals


multivariateNormal :: [Double] -> Matrix Double -> DRandom [Double]
multivariateNormal mu c = DRandom $ \gen ->
  let cholesky = chol (trustSym c)
      xs = take (length mu) $ sample gen (normal 0 1)
      ys = cholesky #> fromList xs
  in (zipWith (+) mu (toList ys))

wiener :: Double -> Int -> DRandom [Double]
wiener t n = DRandom $ \gen ->
  let dt = t / fromIntegral n
      steps = take n $ sample gen (normal 0 (sqrt dt))
      path = scanl (+) 0 steps
  in path

poissonProcess :: Double -> Double -> Int -> DRandom [Int]
poissonProcess lambda t n = DRandom $ \gen ->
  let dt = t / fromIntegral n
      steps = take n $ sample gen (poisson (lambda * dt))
      path = scanl (+) 0 steps
  in path

brownianBridge :: (Double, Double) -> (Double, Double) -> Int -> DRandom [Double]
brownianBridge (_, x1) (t, yt) h = do
  wt <- wiener t h
  let bridge = zipWith (\x i -> x1 + x - (i/t) * (last wt - yt + x1)) wt [0, dt..t]
      dt = t / fromIntegral (h - 1)
  return bridge

-- UTILITIES --

-- list of n rngs with no repeat seeds
nGens n gen = map (\seed -> (mkStdGen seed)) $ take n $ nub $ randomRs (0, 10000) gen

average xs = sum xs / fromIntegral (length xs)

dFromIntegral :: DRandom Int -> DRandom Double
dFromIntegral d = fmap fromIntegral d

dFromBool :: DRandom Bool -> DRandom Double
dFromBool d = fmap (\x -> if x == True then 1.0 else 0.0) d

sample :: StdGen -> DRandom a -> [a]
sample gen d = unfoldr (\g -> Just (run d g, snd (split g))) gen

sampleMean :: Int -> StdGen -> DRandom Double -> Double
sampleMean n gen dist = sum (take n (sample gen dist)) / fromIntegral n

sampleVariance :: Int -> StdGen -> DRandom Double -> Double
sampleVariance n gen dist =
  let samples = take n (sample gen dist)
      mean = sum samples / fromIntegral n
      variance = sum (map (\x -> (x - mean) ^ 2) samples) / fromIntegral (n - 1)
  in variance

sampleMeanVector :: [[Double]] -> [Double]
sampleMeanVector xs =
  let n = fromIntegral $ length xs
  in map (/ n) (foldl1 (zipWith (+)) xs)

averagePath :: Fractional a => [[a]] -> [a]
averagePath lists = map average' (transpose lists)
  where
    average' xs = sum xs / fromIntegral (length xs)

-- shortcut for taking one sample
sample' :: StdGen -> DRandom a -> a
sample' g dist = head $ take 1 $ sample g dist

-- halve the number of steps
reduce :: Num c => [c] -> [c]
reduce path = zipWith (+) (fst $ split' path) (snd $ split' path)
  where
    split' :: [a] -> ([a], [a])
    split' = \list -> case list of
            [] -> ([], [])
            x:xs -> let (evens, odds) = split' xs in (x:odds, evens)

-- split a DRandom [Double] at a given index
dSplitAt :: Int -> DRandom [Double] -> DRandom ([Double], [Double])
dSplitAt i dist = do
  values <- dist
  let (before, after) = splitAt i values
  return (before, after)

-- takes left riemann sum of a given process (discretized on a given interval)
ito :: Num a => StdGen -> DRandom [a] -> a
ito g process = 
  let wt = sample' g process
      reimann = zipWith (\w1 w2 -> w1 * (w2 - w1)) wt (tail wt)
  in (sum reimann)

-- takes midpoint riemann sum of a given process (discretized on a given interval)
stratonovich :: Fractional a => StdGen -> DRandom [a] -> a
stratonovich g process =
  let wt = sample' g process
      midpoints = zipWith (\w1 w2 -> 0.5 * (w1 + w2)) wt (tail wt)
      reimann = zipWith (\w i -> w * (wt !! (i + 1)) - w * (wt !! i)) (midpoints) [0..(length wt - 1)]
  in (sum reimann)

-- (a, b) : interval
-- h : step length
-- xs : input list
histogram :: (Ord a, Fractional a, Enum a) => (a, a) -> a -> [a] -> [Double]
histogram (a, b) h xs = map normalize $ map count partitions
  where
    count histogram = fromIntegral $ length $ filter (\x -> x >= histogram && x < histogram + h) xs
    total = fromIntegral (length partitions)
    normalize count = count / total
    partitions = [a, a + h .. b - h]
