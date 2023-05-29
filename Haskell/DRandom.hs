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
    sample,
    sampleMean,
    sampleVariance
  ) where

-- TODO
-- eventually better to replace with built-in Data.Random distributions

import System.Random hiding (uniform, sample, random)
import Class

-- Define the monad DRandom.

newtype DRandom a = DRandom { run :: StdGen -> (a, StdGen) }

instance Functor DRandom where
  fmap f (DRandom action) = DRandom $ \gen ->
    let (x, gen') = action gen
    in (f x, gen')

instance Applicative DRandom where
  pure x = DRandom $ \gen -> (x, gen)
  (DRandom f) <*> (DRandom x) = DRandom $ \gen ->
    let (f', gen') = f gen
        (x', gen'') = x gen'
    in (f' x', gen'')

instance Monad DRandom where
  return = pure
  (DRandom x) >>= f = DRandom $ \gen ->
    let (x', gen') = x gen
        (DRandom y) = f x'
    in y gen'

-- Define DRandom as an instance of Distribution.

instance Distribution DRandom where

  random :: DRandom Double
  random = DRandom $ randomR (0.0, 1.0)

  uniformDiscrete :: [a] -> DRandom a
  uniformDiscrete xs = DRandom $ \gen ->
    let (i, gen') = randomR (0, length xs - 1) gen
    in (xs !! i, gen')

  bernoulli :: Double -> DRandom Bool
  bernoulli p = DRandom $ \gen ->
    let (x, gen') = randomR (0, 1) gen
    in (x <= p, gen')

  binomial :: Int -> Double -> DRandom Int
  binomial n p = DRandom $ \gen -> binom n p gen 0 0
    where
      binom n p gen count successes
        | count == n = (successes, gen)
        | otherwise =
            let (x, gen') = randomR (0, 1) gen
                successes' = if x <= p then successes + 1 else successes
            in binom n p gen' (count + 1) successes'

  geometric :: Double -> DRandom Int
  geometric p = DRandom $ \gen -> geom p gen 0
    where
      geom p gen count
        | x <= p = (count, gen')
        | otherwise = geom p gen' (count + 1)
        where
          (x, gen') = randomR (0, 1) gen

  poisson :: Double -> DRandom Int
  poisson lambda = DRandom $ \gen -> pois lambda gen 0 1.0
    where
      pois lambda gen count prod
        | prod < (exp (-lambda)) = (count, gen')
        | otherwise = pois lambda gen' (count + 1) (prod * u)
        where
          (u, gen') = randomR (0, 1) gen

  uniform :: Double -> Double -> DRandom Double
  uniform a b = DRandom $ \gen ->
    let (x, gen') = randomR (a, b) gen
    in (x, gen')

  exponential :: Double -> DRandom Double
  exponential lambda = DRandom $ \gen ->
    let (x, gen') = randomR (0.0, 1.0) gen
        val = -log x / lambda
    in (val, gen')

  gamma :: Double -> Double -> DRandom Double
  gamma alpha beta = DRandom $ \gen ->
    let (x, gen') = randomR (0.0, 1.0) gen
        val = -log x / alpha ** (1 / beta)
    in (val, gen')

  normal :: Double -> Double -> DRandom Double
  normal mean stdDev = DRandom $ \gen ->
    let (x, gen') = randomR (0.0, 1.0) gen
        val = mean + stdDev * (sqrt (-2.0 * log x) * cos (2.0 * pi * x))
    in (val, gen')


-- UTILITIES --

sample :: DRandom a -> Int -> IO [a]
sample d n = do
  gen <- newStdGen
  let samples = take n $ map fst $ iterate (run d . snd) (run d gen)
  return samples

sampleMean :: DRandom Double -> Int -> IO Double
sampleMean d n = do
  samples <- sample d n
  let total = sum samples
      mean = total / fromIntegral n
  return mean

sampleVariance :: DRandom Double -> Int -> IO Double
sampleVariance d n = do
  mean <- sampleMean d n
  samples <- sample d n
  let sigmas = map (\x -> (x - mean) ^ 2) samples
      variance = sum sigmas / fromIntegral (n - 1)
  return variance
