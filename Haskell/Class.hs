module Class  ( Distribution,
    random,
    uniformDiscrete,
    bernoulli,
    binomial,
    geometric,
    poisson,
    uniform,
    exponential,
    gamma,
    normal
  ) where

-- Monads that draw random variables
class Monad m => Distribution m where
  
  -- draw from uniform (0, 1)
  random :: m Double

  uniformDiscrete :: [a] -> m a
  uniformDiscrete xs = do
    i <- fmap (round . (* fromIntegral (length xs))) random
    return (xs !! i)

  bernoulli :: Double -> m Bool
  bernoulli p = fmap (< p) random

  binomial :: Int -> Double -> m Int
  binomial n p = fromPMF mass
    where mass x = fromIntegral (n `choose` x) * (p^x) * ((1-p)^(n-x))
          choose :: Int -> Int -> Int
          choose n r = product [fromIntegral (n - r + 1) .. fromIntegral n] `div` product [1 .. r]

  geometric :: Double -> m Int
  geometric p = fromPMF mass
    where mass x = (1 - p)^(x-1) * p

  poisson :: Double -> m Int
  poisson lambda = fromPMF mass
    where mass x = (lambda^x) * exp (-lambda) / fromIntegral (factorial x)
          factorial n = product [1..n]

  uniform :: Double -> Double -> m Double
  uniform a b = fmap (density a b) random
    where
      density a b x
        | x >= a && x <= b = 1 / (b - a)
        | otherwise = 0
  
  exponential :: Double -> m Double
  exponential lambda = fmap (density lambda) random
    where
      density lambda x
        | x >= 0 = lambda * exp (-lambda * x)
        | otherwise = 0 
  
  gamma :: Double -> Double -> m Double
  gamma alpha beta = fmap (density alpha beta) random
    where
      density alpha beta x
        | x >= 0 = (beta ** alpha) / (product [1..(alpha - 1)]) * (x ** (alpha - 1)) * exp (-beta * x)
        | otherwise = 0
  
  normal :: Double -> Double -> m Double
  normal mu sigma = fmap (density mu sigma) random
    where
      density mu sigma x = exp (-((x - mu) ** 2) / (2 * sigma ** 2)) / (sqrt (2 * pi * sigma ** 2))


-- draws from pmf using successive draws from bernoulli
fromPMF :: Distribution m => (Int -> Double) -> m Int
fromPMF pmf = f 0 1
  where
    f i r = do
      let q = pmf i
      b <- bernoulli (q / r)
      if b then pure i else f (i + 1) (r - q)
