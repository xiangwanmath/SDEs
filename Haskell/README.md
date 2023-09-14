# Class Module

This module provides a set of monadic functions for drawing random variables from various probability distributions. The module defines the `Distribution` class, which is a subclass of the `Monad` typeclass, allowing for the use of monadic operations such as `do` notation and the `fmap` function. 

The following distributions are provided:

- `random`: Draws a random number from a uniform distribution over the interval (0, 1).
- `uniformDiscrete`: Draws a random element from a given list with equal probability for each element.
- `bernoulli`: Draws a random boolean value with the given probability of success.
- `binomial`: Draws a random integer from a binomial distribution with the given number of trials and success probability.
- `geometric`: Draws a random integer from a geometric distribution with the given success probability.
- `poisson`: Draws a random integer from a Poisson distribution with the given lambda parameter.
- `uniform`: Draws a random number from a uniform distribution over the given interval.
- `exponential`: Draws a random number from an exponential distribution with the given lambda parameter.
- `gamma`: Draws a random number from a gamma distribution with the given alpha and beta parameters.
- `normal`: Draws a random number from a normal distribution with the given mean and standard deviation.

You need to define an instance of the `Distribution` class for your desired monad; this involves implementing the `random` function and optionally overriding the default implementations for other distribution functions if necessary. The primary purpose of this module is to be used by the following modules `DList`, `DCont`, `DRandom`, which define instances of the `Distribution` class for a few interesting mondadic representations of probability distributions. 

## Examples

```haskell
import Control.Monad.State
import System.Random
import Class

-- Define an instance of the `Distribution` class for the `State StdGen` monad, which allows us to 
-- generate random values using the `randomR` function from the `System.Random` module.
instance Distribution (State StdGen) where
  random = state randomR

-- Roll a fair six-sided die. 
rollDie :: State StdGen Int
rollDie = uniformDiscrete [1 .. 6]

-- Print a randomly drawn result.
main :: IO ()
main = do
  gen <- newStdGen
  let (result, _) = runState rollDie gen
  putStrLn $ "Rolled: " ++ show result
```
```console
ghci> main
Rolled: 5
```

# DRandom Module

The `DRandom` module represents probability distributions via random sampling; it encapsulates the randomness using the `StdGen` type from the `System.Random` library. 

> **Note:** It will probably be better eventually to replace this with the built-in distributions from `Data.Random` or a similar library.

| Requires       |                            |
|----------------|----------------------------|
| `random`       | `cabal install --lib random`|
| `hmatrix`   | `cabal install --lib hmatrix`|

## Functions

The `DRandom` instance defines all the probability distributions in `Distribution` class, as well as the following distributions:

- `multivariateNormal :: [Double] -> Matrix Double -> DRandom [Double]`: Draws a vector of random numbers from a normal distribution with the given mean vector and covariance matrix. 
- `wiener :: Double -> Int -> DRandom [Double]`: Draws an approximate random path from a wiener process with a given end time and number of steps. 
- `brownianBridge :: (Double, Double) -> (Double, Double) -> Int -> DRandom [Double]`: Draws an approximate random path from a brownian bridge from an inital point `(0, x1)` to an endpoint `(t, yt)` with the given number of steps. 

It also defines the following utility functions:

- `histogram :: (Ord a, Num a, Enum a) => (a, a) -> a -> [a] -> [Int]`: Partitions a given interval into subintervals of the provided length, then counts the number of values from a given list (presumably samples from a distribution) which fall into each subinterval.
- `dFromIntegral :: DRandom Int -> DRandom Double`: Converts a `DRandom Int` to a `DRandom Double`. Useful for type wrangling, including calls to `sampleMean` and `sampleVariance`.
- `dFromBool :: DRandom Bool -> DRandom Double`: Converts a `DRandom Bool` to a `DRandom Double`. Useful for type wrangling, including calls to `sampleMean` and `sampleVariance`.
- `sample :: StdGen -> DRandom a -> [a]`: Generates an infinite list of samples from a given distribution, using the random number generator of your choice.
- `sample' :: StdGen -> DRandom a -> a`: Shortcut for taking a single sample from a given distribution, using the random number generator of your choice.
- `sampleMean :: Int -> StdGen -> DRandom Double -> Double`: Calculates the mean of a given distribution with sample size `n`, using the random number generator of your choice.
- `sampleVariance :: Int -> StdGen -> DRandom Double -> Double`: Calculates the variance of a given distribution with sample size `n`, using the random number generator of your choice.
- `sampleMeanVector :: [[Double]] -> [Double]`: Calculates the sample mean vector of a given multivariate distribution.
- `averagePath :: Fractional a => [[a]] -> [a]`: Calculates the average list from a list of lists.
- `reduce :: Num c => [c] -> [c]`: Reduces the number of steps in a path by half. Useful for comparing coarser approximations of the same sample path. 


## Examples

Example probabilistic program:  
```haskell
import Class
import DRandom

-- Generate a random positive even number from a geometric distribution
randEven :: DRandom Int
randEven = (geometric 0.5) >>= \x ->
  let number = if even x then x else x + 1
  in return number

-- Take a sample of five
main :: IO ()
main = do
  g <- newStdGen
  samples <- take 5 (sample g randEven)
  putStrLn $ "Samples: " ++ show samples
```
```console
ghci> main
Samples: [0,0,0,2,0]
```

Example plot: 
```console
ghci> g <- newStdGen
ghci> samples = take 10000 (sample g (normal 0 1))
ghci> h = histogram (-3, 3) 0.05 samples
ghci> plot [h]
1.95 ┼                                                       ╭╮
1.81 ┤                                                       │╰╮   ╭╮
1.67 ┤                                                    ╭─╮│ ╰──╮│╰╮   ╭╮
1.53 ┤                                             ╭╮ ╭╮ ╭╯ ╰╯    ╰╯ │ ╭─╯│
1.39 ┤                                            ╭╯│ │╰─╯           ╰─╯  ╰────╮
1.25 ┤                                          ╭─╯ ╰─╯                        │ ╭╮
1.11 ┤                                      ╭───╯                              ╰─╯╰╮
0.98 ┤                                    ╭─╯                                      ╰────╮
0.84 ┤                                   ╭╯                                             ╰─╮
0.70 ┤                              ╭╮ ╭─╯                                                ╰─╮╭╮
0.56 ┤                          ╭╮╭╮│╰─╯                                                    ╰╯│╭─╮╭╮
0.42 ┤                     ╭╮ ╭─╯╰╯╰╯                                                         ╰╯ ╰╯│╭╮
0.28 ┤                 ╭╮╭─╯╰─╯                                                                    ╰╯╰╮╭──╮
0.14 ┤        ╭────────╯╰╯                                                                            ╰╯  ╰─────────╮╭╮
0.00 ┼────────╯                                                                                                     ╰╯╰──────
```

```console
ghci> plot $ [sample' (mkStdGen 30) $ brownianBridge (0, 0) (1, -1) 100]
 0.03 ┼──╮                        ╭╮
-0.08 ┤  ╰╮    ╭╮              ╭──╯│     ╭─╮╭╮ ╭╮
-0.20 ┤   ╰╮ ╭─╯│      ╭──╮╭╮╭╮│   ╰─╮  ╭╯ ╰╯╰─╯╰╮
-0.31 ┤    ╰╮│  ╰╮ ╭╮╭─╯  ╰╯╰╯╰╯     │╭─╯        ╰╮
-0.42 ┤     ╰╯   ╰─╯╰╯               ╰╯           ╰╮
-0.54 ┤                                            ╰╮     ╭─╮        ╭╮
-0.65 ┤                                             ╰╮╭───╯ ╰╮       │╰──╮ ╭╮
-0.76 ┤                                              ╰╯      ╰───╮  ╭╯   ╰─╯│
-0.87 ┤                                                          ╰╮╭╯       ╰───╮╭╮            ╭╮╭────╮╭─╮
-0.99 ┤                                                           ╰╯            ╰╯│            │╰╯    ╰╯ ╰
-1.10 ┤                                                                           ╰─╮         ╭╯
-1.21 ┤                                                                             ╰╮    ╭───╯
-1.33 ┤                                                                              │   ╭╯
-1.44 ┤                                                                              ╰╮ ╭╯
-1.55 ┤                                                                               ╰─╯

```

Further examples given in `/Examples/Linear.hs`, `/Examples/MeanExitTime.hs`

# DList Module

The `DList` module provides a monadic representation of probability distributions as a list of outcomes tagged with their probabilities, as an instance of the `Distribution` class.

| Requires       |                            |
|----------------|----------------------------|
| `containers`  | `cabal install --lib containers`|

## Functions

The `DList` instance defines all the probability distributions in `Distribution` class, as well as the following:

- `randomWalk :: Eq a => Int -> (a -> DList a) -> DList a -> DList a`: Describes the probability of a random walk being in any of its possible states after time `n`, using a given step function, and starting at a givin inital state.

You can use the given step functions:

- `oneD :: Double -> DList Double`: Simple one-dimsensional random walk.
- `twoD :: (Int, Int) -> DList (Int, Int)`: Simple two-dimensional random walk.

It also provides the following:

- `empirical :: Ord a => [a] -> DList a`: Converts a list of (presumabley randomly sampled) values to a `DList` with the appropriate observed probabilities. For infinte/continuous distributions this can be a much better representation than the default functions.
- `probability :: DList a -> Event a -> Probability`: Calculates the probability of a given `Event a = a -> Bool`
- `expectation :: DList Double -> Double`: Calculates the expectation of a given distribution. 
- `variance :: DList Double -> Double`: Calculates the variance of a given distribution. 


## Examples

You can perform monadic operations on distributions:
```haskell
-- Bind distributions
dist >>= (\x -> uniformDiscrete [x, x + 1, x + 2])

-- Map over a distribution
fmap (\x -> x * 2) dist
```

Probability of an event:
```console
ghci> -- Probability of greater than 15 successes, given a success rate of 80% and 20 trials
ghci> probability (binomial 20 0.8) (> 15)
P 0.62964826390267
```

Example probabilistic program:  
```haskell
import Class
import DList

main :: IO ()
main = do
  -- Create a distribution of coin toss outcomes
  let coinToss = uniformDiscrete [True, False]

  -- Create a random walk of length 10 with coin tosses as steps
  let path = randomWalk 10 (\x -> coinToss) (pure True)

  -- Calculate the probability of reaching False
  let reachFalse = probability path (== False)

  -- Print the probability
  putStrLn $ "probability of reaching False: " ++ show reachFalse
```
```console
ghci> main
probability of reaching False: 0.5
```

Example plot:
```console
ghci> plot [map snd $ toList' $ gamma 5 1]
0.20 ┼   ╭╮
0.18 ┤   │╰╮
0.17 ┤  ╭╯ │
0.15 ┤  │  │
0.14 ┤  │  ╰╮
0.13 ┤  │   │
0.11 ┤  │   │
0.10 ┤  │   ╰╮
0.08 ┤ ╭╯    │
0.07 ┤ │     │
0.06 ┤ │     ╰╮
0.04 ┤ │      │
0.03 ┤ │      ╰╮
0.01 ┤╭╯       ╰─╮
0.00 ┼╯          ╰────────────────────────────────────────────────────────────────────────────────────────
```

# DCont Module

The `DCont` module provides a representation of probability distributions as weighted summation/integration; essentially is the same as the Continuation monad.

> **Note:** It's not yet defined as an instance of `Distribution` and the types are messy.

| Requires       |                            |
|----------------|----------------------------|
| `integration`  | `cabal install --lib integration`|


## Functions

The `DCont` instance defines all the probability distributions in `Distribution` class, as well as the following:

- `probability :: Prob a -> (a -> Bool) -> Double`: Calculates the probability of a given event (`Prob = DCont Double`).
- `expectation :: Prob Double -> Double`: Calculates the expectation of a given distribution. 
- `variance :: Prob Double -> Double`: Calculates the variance of a given distribution. 
- `momentGeneratingFunction :: Prob Double -> Double -> Double`: Defines the moment generating function for a given distribution.

The above functions may be unstable due to quadrature instability.

## Examples

Combining distributions:
```haskell
-- Combine two distributions using the applicative style
dist :: Prob (Double, Double)
dist = (,) <$> poisson 0.3 <*> normal 0 1
```

Probability of an event over some interesting distribution: 
```console
ghci> probability (do {x <- (uniform 0 1); y <- (uniform 0 1); return (x*y)}) (<= 0.5)
0.8521479765473723
```

Example probabilistic program:  
```haskell
import Class
import DCont

main :: IO ()
main = do
  -- Roll two die
  roll1 <- uniformDiscrete [1..6]
  roll2 <- uniformDiscrete [1..6]

  -- Calculate the expected value of the sum of the rolls
  let sum' = expectation (roll1 + roll2)

  -- Print
  putStrLn $ "Expected sum: " ++ show sum'
```
```console
ghci> main
Expected sum: 7.0
```

# Examples/RandomWalk

| Requires       |                            |
|----------------|----------------------------|
| `MonadRandom`  | `cabal install --lib MonadRandom`|

The `RandomWalk` module simulates various random walks. 

## Functions

### Simple Random Walks

- `oneD :: RandomGen g => Int -> Rand g [Int]`: Simulates a one-dimensional random walk  of length `n` by generating a list of steps.

- `twoD :: RandomGen g => Int -> Rand g [(Int, Int)]`: Simulates a two-dimensional random walk  of length `n` by generating a list of (x, y) coordinates.

### Continuous Random Walks

- `oneDC :: RandomGen g => Int -> Rand g [Double]`: Simulates a one-dimensional continuous random walk  of length `n` by generating a list of steps.

- `twoDC :: RandomGen g => Int -> Rand g [(Double, Double)]`: Simulates a two-dimensional continuous random walk  of length `n` by generating a list of (x, y) coordinates.

### Graphing

- `plotavg :: Int -> Int -> IO ()`: Plots the average of `k` random walk paths of length `n`.

## Examples

```console
ghci> :load randomWalk.hs
[1 of 1] Compiling RandomWalk       ( randomWalk.hs, interpreted )
Ok, one module loaded.

-- Two-dimensional random walk of length 10, using random seed
ghci> g <- newStdGen
ghci> evalRand (twoD 10) g
[(-1,1),(0,0),(-1,-1),(0,0),(1,1),(0,2),(1,3),(0,2),(1,1),(0,0),(-1,1)]

-- One-dimensional continuous random walk, using random number generator mkStdGen with seed 42
ghci> evalRand (oneDC 10) (mkStdGen 42)
[0.590845872954454,-0.2793598223582849,-0.6029027491509255,-0.16981096350677124,-1.0584093405062047,-1.1160996148112077,-1.8716156622889896,-2.246456383219449,-1.755405687556058,-1.779212304261348]

-- Plot a one-dimensional simple random walk of length 100
ghci> g <- newStdGen
ghci> path = map fromIntegral $ evalRand (oneD 100) g
ghci> plot [path]
 9.00 ┤                                         ╭╮                                  ╭╮
 8.29 ┤                                         ││                                  ││
 7.57 ┤                                        ╭╯╰╮    ╭╮                          ╭╯╰╮
 6.86 ┤                                   ╭╮  ╭╯  ╰╮╭╮╭╯╰╮╭╮╭╮  ╭╮                ╭╯  ╰╮  ╭╮      ╭╮      ╭
 6.14 ┤                                   ││  │    ││││  │││││  ││                │    │  ││      ││      │
 5.43 ┤                          ╭╮╭╮    ╭╯╰╮╭╯    ╰╯╰╯  ╰╯╰╯╰╮╭╯╰╮              ╭╯    ╰╮╭╯╰╮╭╮  ╭╯╰╮    ╭╯
 4.71 ┤       ╭╮╭╮              ╭╯╰╯╰╮  ╭╯  ╰╯                ╰╯  ╰╮      ╭╮╭╮  ╭╯      ╰╯  ╰╯╰╮╭╯  ╰╮╭╮╭╯
 4.00 ┤      ╭╯╰╯╰╮            ╭╯    ╰╮╭╯                          ╰╮╭╮╭╮╭╯╰╯╰╮╭╯              ╰╯    ╰╯╰╯
 3.29 ┤      │    │            │      ││                            ││││││    ││
 2.57 ┤     ╭╯    ╰╮          ╭╯      ╰╯                            ╰╯╰╯╰╯    ╰╯
 1.86 ┤  ╭╮╭╯      ╰╮  ╭╮    ╭╯
 1.14 ┤  │││        │  ││    │
 0.43 ┤ ╭╯╰╯        ╰╮╭╯╰╮  ╭╯
-0.29 ┤╭╯            ╰╯  ╰╮╭╯
-1.00 ┼╯                  ╰╯


-- Plot the average of 100,000 one-dimensional simple random walks of length 100
ghci> plotavg 100 100000
 0.02 ┤
 0.02 ┤
 0.02 ┤
 0.01 ┤
 0.01 ┤
 0.01 ┤
 0.01 ┤                               ╭─╮
 0.01 ┤                             ╭─╯ │
 0.01 ┤      ╭╮   ╭──╮  ╭──╮        │   ╰╮      ╭──╮
 0.01 ┤      │╰─╮ │  ╰─╮│  ╰╮    ╭╮╭╯    │ ╭─╮  │  ╰╮
 0.01 ┤     ╭╯  ╰─╯    ╰╯   │ ╭──╯╰╯     ╰─╯ ╰─╮│   │╭─╮
 0.01 ┤     │               ╰─╯                ╰╯   ╰╯ ╰───╮                                        ╭───╮ ╭
 0.01 ┤  ╭──╯                                              ╰╮                                       │   ╰╮│
 0.00 ┼──╯                                                  ╰╮                                ╭╮   ╭╯    ╰╯
 0.00 ┤                                                      │             ╭╮                 │╰───╯
 0.00 ┤                                                      │             ││╭───╮           ╭╯
 0.00 ┤                                                      ╰╮╭╮       ╭──╯╰╯   │      ╭╮╭╮╭╯
 0.00 ┤                                                       ╰╯╰╮   ╭──╯        ╰─╮╭╮  │╰╯╰╯
-0.00 ┤                                                          │   │             ╰╯│ ╭╯
-0.00 ┤                                                          ╰╮  │               ╰─╯
-0.00 ┤                                                           ╰──╯
-0.00 ┤
-0.00 ┤
-0.01 ┤
-0.01 ┤
```

# Chart Module

Modified from `Data.Text.Chart`, this module provides a simple interface for plotting to the terminal. 

## Functions

- `plot :: [[Double]] -> IO ()`: Takes a list of `[Double]` lists and prints out a corresponding chart. Uses the default `Options`: terminal `height` 14, `minY` and `maxY` are the minimum and maximum values of the input list, "line" `style`. Color cycle is `Nothing`, blue, red, green, magenta, yellow, cyan.

- `plotWith :: Options -> [[Double]] -> IO ()`: Same as `plot`, but allows customizing the chart options. The `Options` are described below.

- `makeLegend :: [([Char], Maybe String)] -> [Char]`: Given a list of strings with associated color keys, returns a textbox legend displaying the strings in the proper colors. See first example. 

- `getANSI :: Maybe String -> String`: Gets the associated ANSI color code.

- `resetCode :: String`: Shortcut for ANSI reset code. 

- `contour :: [[Double]] -> IO ()`: Takes a grid `[[Double]]` of z-values, and plots a contour map.

- `contourWith :: [String] -> [[Double]] -> IO ()`: Same as `contour`, but allows custom palettes.

- `contourF :: (Int, Int) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double) -> IO ()`: Plots a contour map, given the number of steps on the x and y axes `(Int, Int)`, the range of x values `(Double, Double)`, the range of y values `(Double, Double)`, and a function in terms of z `(Double -> Double -> Double)`.

- `contourFWith :: [String] -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double) -> IO ()`: Same as `contourF`, but allows custom palettes.

- `colorPalette :: [String]`: the default color gradient for contour maps.

- `monochromePalette :: [String]`: a monochrome gradient for contour maps. 

## Options

The `Options` type provides a way to customize the appearance of the chart. It has the following fields:

- `height :: Double`: Height of the chart.

- `style :: String`: Style of the plot. Can be either "line" or "points".

- `colors :: [Maybe String]`: Color of the line at the associated index. If there are not enough colors provided, the list will cycle. Choose from: 
    - `Nothing` (default terminal settings)
    - `Just "black"`
    - `Just "red"`
    - `Just "green"`
    - `Just "yellow"`
    - `Just "blue"`
    - `Just "magenta"`
    - `Just "cyan"`
    - `Just "white"`

> **Note:** A few issues with the following: Use `Just` notation (see last example), will wrap values by default later. For now just to get an idea of scale, don't try to set min or max values within the range of your list (ie cut off some values).

- `minY :: Maybe Double`: Minimum Y value.

- `maxY :: Maybe Double`: Maximum Y value.

## Examples

```haskell
main :: IO ()
main = do
  let f = (10.0 *) . sin . (/ 120.0) . (pi *) . (4.0 *) <$> [0..60.0]
      g = (10.0 *) . cos . (/ 120.0) . (pi *) . (4.0 *) <$> [0..60.0]
      key = [("f(x)", Just "red"), ("g(x)", Just "blue")]

  putStrLn $ makeLegend key
  plotWith options { colors = map snd key } [f, g]
```
```console
ghci> main
            +------+
            | f(x) |
            | g(x) |
            +------+
       
 10.00 ┤───╮       ╭──────╮                                     ╭───
  8.57 ┤   ╰──╮ ╭──╯      ╰──╮                               ╭──╯
  7.14 ┤      ╰─╮            ╰─╮                           ╭─╯
  5.71 ┤     ╭╯ ╰─╮            ╰╮                        ╭─╯
  4.29 ┤   ╭─╯    ╰╮            ╰─╮                     ╭╯
  2.86 ┤  ╭╯       ╰╮             ╰╮                   ╭╯
  1.43 ┤╭─╯         ╰─╮            ╰─╮               ╭─╯
  0.00 ┼╯             ╰╮             ╰╮             ╭╯             ╭
 -1.43 ┤               ╰─╮            ╰─╮         ╭─╯            ╭─╯
 -2.86 ┤                 ╰╮             ╰╮       ╭╯             ╭╯
 -4.29 ┤                  ╰─╮            ╰─╮    ╭╯            ╭─╯
 -5.71 ┤                    ╰╮             ╰╮ ╭─╯            ╭╯
 -7.14 ┤                     ╰─╮            ╭─╯            ╭─╯
 -8.57 ┤                       ╰──╮      ╭──╯ ╰──╮      ╭──╯
-10.00 ┤                          ╰──────╯       ╰──────╯

```

```console
ghci> :load DRandom.hs 
[1 of 2] Compiling Chart            ( Chart.hs, interpreted )
[2 of 2] Compiling DRandom          ( DRandom.hs, interpreted )
Ok, two modules loaded.
ghci> b1 = brownianBridge (0, 1) (1, 0) 100
ghci> b2 = brownianBridge (0, (-1)) (1, 0) 100
ghci> plot [sample' (mkStdGen 30) b1, sample' (mkStdGen 50) b2]
 1.03 ┼─╮                         ╭╮
 0.88 ┤ ╰─╮    ╭╮              ╭──╯│     ╭─╮╭╮
 0.72 ┤   ╰╮ ╭─╯│  ╭╮  ╭──╮╭───╯   ╰─╮  ╭╯ ╰╯╰───╮
 0.57 ┤    ╰─╯  ╰╮╭╯╰──╯  ╰╯         ╰──╯        ╰─╮
 0.42 ┤          ╰╯                                ╰╮     ╭─╮        ╭╮
 0.27 ┤                                             ╰─────╯ ╰╮       │╰──╮ ╭╮        ╭╮     ╭──────╮
 0.12 ┤                                                      ╰────╮╭─╯   ╰─╯╰───╮╭╮ ╭╯╰╮╭───╯  ╭╮╭─╰──────
-0.04 ┤                                                           ╰╯            ╰╭──╯  ╰╯      │╰╯  ╰╯╰╯ ╰
-0.19 ┤                                                                       ╭╮╭╯╰─╮      ╭╮╭─╯
-0.34 ┤              ╭╮                                        ╭─────╮   ╭╮ ╭─╯╰╯   ╰╮    ╭╯╰╯
-0.49 ┤             ╭╯╰╮╭──╮                      ╭──╮╭──╮   ╭─╯     ╰───╯│╭╯        ╰╮ ╭─╯
-0.65 ┤        ╭─╮╭─╯  ╰╯  ╰─╮     ╭╮             │  ╰╯  ╰───╯            ╰╯          ╰─╯
-0.80 ┤    ╭───╯ ╰╯          ╰──╮ ╭╯╰─────────────╯
-0.95 ┤ ╭╮ │                    ╰─╯
-1.10 ┤─╯╰─╯

```

```console
ghci> :load randomWalk.hs 
[1 of 2] Compiling Chart            ( Chart.hs, interpreted )
[2 of 2] Compiling RandomWalk       ( randomWalk.hs, interpreted )
Ok, two modules loaded.
ghci> g <- newStdGen
ghci> path = map fromIntegral $ evalRand (oneD 100) g
ghci> plotWith options {style = "points"} [path]
 9.00 ┤                                                                                        *
 8.00 ┤                                                                                       * *
 7.00 ┤                                                                                      *   *
 6.00 ┤                                                                                     *     *       *
 5.00 ┤                                                                                    *       * *   * *
 4.00 ┼                                     *                       * *                   *         * * *
 3.00 ┤                      * *     *     * * *               *   * * *           *     *             *
 2.00 ┤ *               * * * * * * * *   *   * * *         * * * *     * *     * * * * *
 1.00 ┼* *             * * *     * *   * *       * *       * *   *       * * * * *   * *
 0.00 ┤   *           *                 *           * * * *                 * *
-1.00 ┤    *         *                               * * *
-2.00 ┤     *       *
-3.00 ┤      *   * *
-4.00 ┤       * * *
-5.00 ┤        *
```

```haskell
import Class
import DRandom
import Chart

main :: IO ()
main = do 
  g <- newStdGen
  let coin = bernoulli 0.7
      dist = do
        x <- coin
        normal (if x then (-3) else 3) 1
      samples = take 1000 $ sample g dist 
      hist = histogram (-7, 7) 0.2 samples
  plotWith options { maxY = Just 1 } [hist]
```
```console
ghci> main
1.00 ┼
0.94 ┤
0.90 ┤
0.85 ┤
0.81 ┤             ╭╮
0.77 ┤             │╰╮       ╭╮                       ╭╮╭╮
0.72 ┤             │ │       ││                       │╰╯│
0.68 ┤             │ │       │╰╮                      │  │
0.64 ┤            ╭╯ │╭╮     │ │                      │  │
0.59 ┤            │  ╰╯│    ╭╯ ╰╮                   ╭╮│  ╰╮
0.55 ┤            │    │╭╮╭╮│   ╰╮                  │╰╯   │
0.41 ┤            │    ││││╰╯    │                  │     │
0.36 ┤           ╭╯    ╰╯╰╯      │                  │     │
0.32 ┤           │               ╰─╮               ╭╯     ╰╮
0.27 ┤         ╭─╯                 ╰╮              │       │
0.23 ┤        ╭╯                    │              │       │
0.19 ┤        │                     │              │       │
0.14 ┤     ╭──╯                     ╰───╮         ╭╯       │
0.00 ┼─────╯                            ╰─────────╯        ╰───────────────
```
```console
ghci> contourF (50, 25) (-3, 3) (-3, 5) (\x y -> (x^2 + (y - ((** (1/3)) (x^2)))^2) - 1)
''''''''''''''''''''---:=o=:---''''''''''''''''''''
'',,,,,,,,,,,,,,,,,'''''-=-''''',,,,,,,,,,,,,,,,,''
,,,,,,,---------,,,,,,,''-'',,,,,,,---------,,,,,,,
,,--------------------,,,',,,--------------------,,
--------..........------,,,------..........--------
-----..................-----..................-----
---......................-......................---
--...........       ...........       ...........--
-.........              ...              .........-
-........                .                ........-
-.......                                   .......-
-.......                                   .......-
--......                                   ......--
---......                                 ......---
----......                               ......----
,----......                             ......----,
,,,---.......                         .......---,,,
',,,----.......                     .......----,,,'
'',,,,----.......                 .......----,,,,''
-''',,,,----.......             .......----,,,,'''-
:--''',,,,----.......         .......----,,,,'''--:
;=:--'''',,,-----.......   .......-----,,,''''--:=;
!o;=:--'''',,,,----...... ......----,,,,''''--:=;o!
0X?!o;=:--'''',,,,----.......----,,,,''''--:=;o!?X0
%80X?!o;=:---''',,,,----...----,,,,'''---:=;o!?X08%
$&%#80X?!o;=:--'''',,,,-----,,,,''''--:=;o!?X08#%&@
```

# Benchmarks.hs

This module provides a collection of tests designed for benchmarking purposes.

| Requires       |                            |
|----------------|----------------------------|
| `criterion`  | `cabal install --lib criterion`|
| `MonadRandom`  | `cabal install --lib MonadRandom`|

## Usage

1. Modify the tests as desired.

2. Navigate to the root directory.

3. Compile the `Benchmarks.hs` file:

   ```console
   ghc -O2 Benchmarks.hs
   ```

4. To run the tests, execute either of the following commands:

   - For console output:
     ```console
     ./Benchmarks
     ```

   - For generating an HTML report:
     ```console
     ./Benchmarks --output benchmarks.html
     ```

# Usage

Import the desired module in a `.hs` file, or interact with the modules in a GHCi environment, but you must have GHC and all the required packages installed already:

1. Navigate to the root directory.

2. Start GHCi:

  ```console
   ghci
   ```

3. Load a specific module:

  ```console
   :load <module>.hs
   ```

> **Note:** Will be able to build later with `stack build`.

<!---
## Usage

1. Install [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

2. Clone the repository.

3. Navigate to the root directory of the library.

4. Run the following command to build the library:

   ```
   stack build
   ```
5. To test out your own code or the examples above, edit the `app/Main.hs` file. Then running the following command will show you the ouput of `main` function. 
   ```
   stack exec stochastic-exe
   ```

You can also interact with the modules in a GHCi environment, but you must have GHC, and all the appropriate packages installed already:

1. Navigate to the `src` directory of the library.

2. Start GHCi:

   ```
   ghci
   ```

3. Load a specific module:

   ```
   :load <module>.hs
   ```
-->
