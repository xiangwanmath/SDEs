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
    dFromIntegral,
    sample,
    sampleMean,
    sampleVariance
  ) where

import System.Random hiding (uniform, sample, random)
import Control.Monad
import Data.List (unfoldr)
import Class

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

-- UTILITIES --

dFromIntegral :: DRandom Int -> DRandom Double
dFromIntegral d = fmap fromIntegral d

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
