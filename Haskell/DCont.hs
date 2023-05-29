module DCont ( DCont,
    uniformDiscrete,
    bernoulli,
    binomial,
    geometric,
    poisson,
    uniform,
    exponential,
    gamma,
    normal,
    probability,
    expectation,
    variance,
    momentGeneratingFunction
  ) where

-- Representation of a Distribution as weighted summation/integration.

-- TODO
-- make instance of Distribution
-- properly type, use `fmap fromIntegral` when calling binomial, etc. 
-- runCC equivalent ?? 

import Numeric.Integration.TanhSinh

-- Define the monad DCont.

newtype DCont r a = DCont { run :: (a -> r) -> r }

instance Functor (DCont r) where
  fmap f (DCont g) = DCont $ \k -> g (k . f)

instance Applicative (DCont r) where
  pure a = DCont ($ a)
  mf <*> mx = DCont $ \k -> run mf (\f -> run mx (k . f))

instance Monad (DCont r) where
  return = pure
  ma >>= f = DCont $ \k -> run ma (\a -> run (f a) k)

type Prob = DCont Double

dist f = DCont $ \k -> run (f (\a -> DCont $ \_ -> k a)) k

-- discrete distributions --

uniformDiscrete :: [a] -> Prob a
uniformDiscrete xs = DCont $ \k -> sum (map (\x -> k x) xs) / fromIntegral (length xs)

bernoulli :: Double -> Prob Double
bernoulli p = DCont $ \k -> k 1.0 * p + k 0.0 * (1 - p)

binomial :: Int -> Double -> Prob Double
binomial n p = DCont $ \k -> sum [k (fromIntegral r) * choose n r * (p ** fromIntegral r) * ((1 - p) ** fromIntegral (n - r)) | r <- [0..n]]
  where
    choose :: Int -> Int -> Double
    choose n r = product [fromIntegral (n - r + 1) .. fromIntegral n] / product [1 .. fromIntegral r]

geometric :: Double -> Prob Double
geometric p = DCont $ \k -> sum [k (fromIntegral r) * (1 - p) ** (fromIntegral (r - 1)) * p | r <- [1..cutoff]]
  where
    cutoff = 1000 -- max number of terms

poisson :: Double -> Prob Double
poisson lambda = DCont $ \k -> sum [k (fromIntegral r) * (exp (-lambda)) * (lambda ** fromIntegral r) / fromIntegral (factorial r) | r <- [0..cutoff]]
  where
    cutoff = 1000
    factorial n = product [1..n]

-- continuous distributions --

fromPDF :: (Double -> Double) -> Prob Double
fromPDF d = DCont $ \f ->
  quadrature (\x -> f x * d x)
  where
    quadrature = result . last . everywhere trap

uniform :: Double -> Double -> Prob Double
uniform a b = fromPDF (density a b)
  where
    density a b x
      | x >= a && x <= b = 1 / (b - a)
      | otherwise = 0

exponential :: Double -> Prob Double
exponential lambda = fromPDF (density lambda)
  where
    density lambda x
      | x >= 0 = lambda * exp (-lambda * x)
      | otherwise = 0

gamma :: Double -> Double -> Prob Double
gamma alpha beta = fromPDF (density alpha beta)
  where
    density alpha beta x
      | x >= 0 = (beta ** alpha) / (gammaFunc alpha) * (x ** (alpha - 1)) * exp (-beta * x)
      | otherwise = 0
    gammaFunc n = product [1..(n - 1)]
        
normal :: Double -> Double -> Prob Double
normal mu sigma = fromPDF (density mu sigma)
  where
    density mu sigma x = (1 / (sigma * sqrt (2 * pi))) * exp (-(x - mu) ** 2 / (2 * sigma ** 2))

-- utilities -- 

probability :: Prob a -> (a -> Bool) -> Double
probability dist p = run dist $ \x -> if p x then 1 else 0

expectation :: Prob Double -> Double
expectation dist = run dist id

variance :: Prob Double -> Double
variance dist = run dist (\x -> (x - mu) ^ 2) where
  mu = expectation dist

momentGeneratingFunction :: Prob Double -> Double -> Double
momentGeneratingFunction nu t = run (fmap (exp . (t *)) nu) id

