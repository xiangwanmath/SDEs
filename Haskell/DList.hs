module DList ( DList,
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
    empirical,
    toList',
    probability,
    expectation,
    variance,
    oneD,
    twoD,
    randomWalk,
  ) where

-- Representation of a Distribution as a list where each outcome is tagged with its probability.
-- I.e., [(1, P_1), (2, P_2), ...]

import Data.List (sort,groupBy,foldl')
import Data.Map (fromListWith, toList)
import Data.Function (on)
import Control.Monad
import Data.Text.Chart

import Class
import Chart

-- Define the monad DList.

newtype Probability = P Double deriving (Num, Show)
newtype DList a = D {dist :: [(a, Probability)]} deriving (Show)

type Event a = a -> Bool

instance Functor DList where
  fmap f (D d) = D [(f x,p) | (x,p) <- d]

instance Applicative DList where
  pure x = D [(x, P 1.0)]
  (D fs) <*> (D xs) = D [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad DList where
  return = pure
  d >>= f  = D [(y,q*p) | (x,p) <- dist d, (y,q) <- dist (f x)]


-- Define DList as an instance of Distribution.

instance Distribution DList where

  random :: DList Double
  random = uniform 0 1

  uniformDiscrete :: [a] -> DList a
  uniformDiscrete xs = D [(x, P (1 / fromIntegral (length xs))) | x <- xs]

  bernoulli :: Double -> DList Bool
  bernoulli p = D [(True, P p), (False, P (1 - p))]

  binomial :: Int -> Double -> DList Int
  binomial n p = D [(fromIntegral x, P (coef n x * p^x * (1-p)^(n-x))) | x <- [0..n]]
    where
      coef n k = product [fromIntegral (n - i + 1) / fromIntegral i | i <- [1..k]]

  -- approximates distribution with ~10/p outcomes
  geometric :: Double -> DList Int
  geometric p = D [(k, P ((1 - p) ** (fromIntegral (k - 1)) * p)) | k <- [1..round (10 / p)]]

  -- approximates distribution with ~10/lambda outcomes
  poisson :: Double -> DList Int
  poisson lambda = D [(k, P (exp (-lambda) * lambda^k / fromIntegral (product [1..k]))) | k <- [0..round (10 / lambda)]]
  
  uniform :: Double -> Double -> DList Double
  uniform a b
    | a <= b = D [(x, P (1 / (b - a))) | x <- [a..b]]
    | otherwise = D []

  -- approximates distribution with 10/lambda outcomes
  exponential :: Double -> DList Double
  exponential lambda
    | lambda > 0 = D [(x, P (lambda * exp (-lambda * x))) | x <- [0..(10/lambda)]]
    | otherwise = D []

  -- approximates distribution with 100 outcomes
  gamma :: Double -> Double -> DList Double
  gamma alpha beta
    | alpha > 0 && beta > 0 = D [(x, P ((beta**alpha * x**(alpha - 1) * exp (-beta * x)) / gammaFunction alpha)) | x <- [0..100]]
    | otherwise = D []
    where
      gammaFunction n = product [1..n-1]

  -- approximates distribution with 50 outcomes
  normal :: Double -> Double -> DList Double
  normal mu sigma = D [(x, P (pdf x)) | x <- sort [mu - 25 * sigma, mu - 24 * sigma .. mu + 25 * sigma]]
    where
      pdf x = exp (-0.5 * ((x - mu) / sigma) ^ 2) / (sigma * sqrt (2 * pi))

-- UTILITIES -- 

-- Converts a list of random draws to a DList.
empirical :: Ord a => [a] -> DList a
empirical xs = D [(x, P (fromIntegral count / total)) | (x, count) <- (toList (fromListWith (+) [(x, 1) | x <- xs]))]
  where
    total :: Double
    total = fromIntegral (length xs)

-- Converts DList to list of tuples.
toList' :: DList a -> [(a, Double)]
toList' (D d) = zip values probabilities
  where
    values = map fst d
    probabilities = map (\(P p) -> p) $ map snd d

probability :: DList a -> Event a -> Probability
probability (D d) event = sum [p | (x, p) <- d, event x]

expectation :: DList Double -> Double
expectation (D d) = sum [x * p | (x, P p) <- d]

variance :: DList Double -> Double
variance (D d) = sum [(x - mu)^2 * p | (x, P p) <- d]
  where
    mu = expectation (D d)

-- RANDOM WALK -- 

oneD :: Double -> DList Double
oneD x = D [(x - 1, P 0.5), (x + 1, P 0.5)]

twoD :: (Int, Int) -> DList (Int, Int)
twoD (x, y) =
  D
    [((x - 1, y), P 0.25), -- W
     ((x + 1, y), P 0.25), -- E
     ((x, y - 1), P 0.25), -- S
     ((x, y + 1), P 0.25)] -- N

-- Returns a probability distribution representing a random walk by
-- binding the given step function directly to a distribution to get
-- the next timestep (`reduce`ing along the way). 
-- n : length of random walk
-- step : step function
-- init : initial state
randomWalk :: Eq a => Int -> (a -> DList a) -> DList a -> DList a
randomWalk n step init = foldl' (reduce') init (replicate n step)
  where reduce' acc f = reduce (acc >>= f)

-- Adds up joint probabilites of two paths leading to the same state to get rid of duplicates.
-- Also makes runtime linear relative to the number states, as opposed to linear relative to 
-- the number of paths (which is exponential relative to n).
reduce :: (Eq a) => DList a -> DList a
reduce (D d) = D $ map combine $ groupBy ((==) `on` fst) d
  where
    combine xs@((state, _):_) = (state, addP xs)
    addP = P . sum . map ((\(P p) -> p) . snd)

