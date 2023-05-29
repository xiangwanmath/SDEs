module Main where

import Criterion.Main
import Control.Monad.Random hiding (uniform)
import Control.Monad.State
import System.Random hiding (uniform)

import Class
import DList
import DCont qualified as DC
import DRandom
import RandomWalk as RW

-- main --

main :: IO ()
main = do
  putStrLn "-| DList |-"
  defaultMain benchmarkDList

  putStrLn "-| DCont |-"
  defaultMain benchmarkDCont

  putStrLn "-| DRandom |-"
  defaultMain benchmarkDRandom

  putStrLn "-| RandomWalk |-"
  defaultMain benchmarkRandomWalk

-- DList --

dListUniformDiscrete :: [Double] -> Benchmark
dListUniformDiscrete xs = bench "uniformDiscrete" $ whnf (probability (uniformDiscrete xs)) (> 0.5)

dListBernoulli :: Double -> Benchmark
dListBernoulli p = bench "bernoulli" $ whnf (probability (bernoulli p)) (== True)

dListBinomial :: Int -> Double -> Benchmark
dListBinomial n p = bench "binomial" $ whnf (probability (binomial n p)) (> 3)

dListGeometric :: Double -> Benchmark
dListGeometric p = bench "geometric" $ whnf (probability (geometric p)) (> 3)

dListPoisson :: Double -> Benchmark
dListPoisson lambda = bench "poisson" $ whnf (probability (poisson lambda)) (> 3)

dListUniform :: Double -> Double -> Benchmark
dListUniform a b = bench "uniform" $ whnf (probability (uniform a b)) (> 0.5)

dListExponential :: Double -> Benchmark
dListExponential lambda = bench "exponential" $ whnf (probability (exponential lambda)) (> 0.5)

dListGamma :: Double -> Double -> Benchmark
dListGamma alpha beta = bench "gamma" $ whnf (probability (gamma alpha beta)) (> 0.5)

dListNormal :: Double -> Double -> Benchmark
dListNormal mu sigma = bench "normal" $ whnf (probability (normal mu sigma)) (> 0.5)

dListEmpirical :: [Double] -> Benchmark
dListEmpirical xs = bench "empirical" $ whnf (probability (empirical xs)) (> 0.5)

dListExpectation :: DList Double -> Benchmark
dListExpectation dlist = bench "expectation" $ whnf expectation dlist

dListVariance :: DList Double -> Benchmark
dListVariance dlist = bench "variance" $ whnf variance dlist

-- dListRandomWalk :: Int -> (Double -> DList Double) -> DList Double -> Benchmark
-- dListRandomWalk n step init = bench "randomWalk" $ nf (\() -> DList.randomWalk n step init) ()

benchmarkDList :: [Benchmark]
benchmarkDList =
  [ dListUniformDiscrete [1..100]
  , dListBernoulli 0.5
  , dListBinomial 100 0.5
  , dListGeometric 0.5
  , dListPoisson 5.0
  , dListUniform 0.0 1.0
  , dListExponential 1.0
  , dListGamma 2.0 3.0
  , dListNormal 0.0 1.0
  , dListEmpirical [1..100]
  , dListExpectation (uniform 0.0 1.0)
  , dListVariance (uniform 0.0 1.0)
--  , dListRandomWalk 1000 DList.oneD (DList.oneD 0.0)
  ]

-- DCont --

dContUniformDiscrete :: [Double] -> Benchmark
dContUniformDiscrete xs = bench "uniformDiscrete" $ whnf (DC.probability (DC.uniformDiscrete xs)) (> 0.5)

dContBernoulli :: Double -> Benchmark
dContBernoulli p = bench "bernoulli" $ whnf (DC.probability (DC.bernoulli p)) (> 0.5)

dContBinomial :: Int -> Double -> Benchmark
dContBinomial n p = bench "binomial" $ whnf (DC.probability (DC.binomial n p)) (> 0.5)

dContGeometric :: Double -> Benchmark
dContGeometric p = bench "geometric" $ whnf (DC.probability (DC.geometric p)) (> 0.5)

dContPoisson :: Double -> Benchmark
dContPoisson lambda = bench "poisson" $ whnf (DC.probability (DC.poisson lambda)) (> 0.5)

dContUniform :: Double -> Double -> Benchmark
dContUniform a b = bench "uniform" $ whnf (DC.probability (DC.uniform a b)) (> 0.5)

dContExponential :: Double -> Benchmark
dContExponential lambda = bench "exponential" $ whnf (DC.probability (DC.exponential lambda)) (> 0.5)

dContGamma :: Double -> Double -> Benchmark
dContGamma alpha beta = bench "gamma" $ whnf (DC.probability (DC.gamma alpha beta)) (> 0.5)

dContNormal :: Double -> Double -> Benchmark
dContNormal mu sigma = bench "normal" $ whnf (DC.probability (DC.normal mu sigma)) (> 0.5)

dContExpectation :: DC.DCont Double Double -> Benchmark
dContExpectation dist = bench "expectation" $ whnf DC.expectation dist

dContVariance :: DC.DCont Double Double -> Benchmark
dContVariance dist = bench "variance" $ whnf DC.variance dist

dContMomentGeneratingFunction :: DC.DCont Double Double -> Benchmark
dContMomentGeneratingFunction dist = bench "momentGeneratingFunction" $ whnf (DC.momentGeneratingFunction dist) 1.0


benchmarkDCont :: [Benchmark]
benchmarkDCont =
  [ dContUniformDiscrete [1..100]
  , dContBernoulli 0.5
  , dContBinomial 100 0.5
  , dContGeometric 0.5
  , dContPoisson 5.0
  , dContUniform 0.0 1.0
  , dContExponential 1.0
  , dContGamma 2.0 3.0
  , dContNormal 0.0 1.0
  , dContExpectation (DC.uniform 0.0 1.0)
  , dContVariance (DC.uniform 0.0 1.0)
  , dContMomentGeneratingFunction (DC.uniform 0.0 1.0)
  ]

-- DRandom --

dRandomUniformDiscrete :: [Int] -> Int -> Benchmark
dRandomUniformDiscrete xs n = bench "uniformDiscrete" $ nfIO $ DRandom.sample (DRandom.uniformDiscrete xs) n

dRandomBernoulli :: Double -> Int -> Benchmark
dRandomBernoulli p n = bench "bernoulli" $ nfIO $ DRandom.sample (DRandom.bernoulli p) n

dRandomBinomial :: Int -> Double -> Int -> Benchmark
dRandomBinomial n p trials = bench "binomial" $ nfIO $ DRandom.sample (DRandom.binomial n p) trials

dRandomGeometric :: Double -> Int -> Benchmark
dRandomGeometric p n = bench "geometric" $ nfIO $ DRandom.sample (DRandom.geometric p) n

dRandomPoisson :: Double -> Int -> Benchmark
dRandomPoisson lambda n = bench "poisson" $ nfIO $ DRandom.sample (DRandom.poisson lambda) n

dRandomUniform :: Double -> Double -> Int -> Benchmark
dRandomUniform a b n = bench "uniform" $ nfIO $ DRandom.sample (DRandom.uniform a b) n

dRandomExponential :: Double -> Int -> Benchmark
dRandomExponential lambda n = bench "exponential" $ nfIO $ DRandom.sample (DRandom.exponential lambda) n

dRandomGamma :: Double -> Double -> Int -> Benchmark
dRandomGamma alpha beta n = bench "gamma" $ nfIO $ DRandom.sample (DRandom.gamma alpha beta) n

dRandomNormal :: Double -> Double -> Int -> Benchmark
dRandomNormal mean stdDev n = bench "normal" $ nfIO $ DRandom.sample (DRandom.normal mean stdDev) n

dRandomSampleMean :: DRandom.DRandom Double -> Int -> Benchmark
dRandomSampleMean d n = bench "sampleMean" $ nfIO $ DRandom.sampleMean d n

dRandomSampleVariance :: DRandom.DRandom Double -> Int -> Benchmark
dRandomSampleVariance d n = bench "sampleVariance" $ nfIO $ DRandom.sampleVariance d n


benchmarkDRandom :: [Benchmark]
benchmarkDRandom =
  [ bgroup "DRandom"
      [ dRandomUniformDiscrete [1..10] 1000000
      , dRandomBernoulli 0.5 1000000
      , dRandomBinomial 10 0.5 1000000
      , dRandomGeometric 0.3 1000000
      , dRandomPoisson 5.0 1000000
      , dRandomUniform 0.0 1.0 1000000
      , dRandomExponential 2.0 1000000
      , dRandomGamma 2.0 1.0 1000000
      , dRandomNormal 0.0 1.0 1000000
      , dRandomSampleMean (DRandom.normal 0 1) 1000000
      , dRandomSampleVariance (DRandom.uniform (-1) 1) 1000000
      ]
  ]

-- randomWalk --

randomWalkOneD :: Int -> Benchmark
randomWalkOneD n = bench "oneD" $ nfIO (evalRandIO (RW.oneD n))

randomWalkTwoD :: Int -> Benchmark
randomWalkTwoD n = bench "twoD" $ nfIO (evalRandIO (RW.twoD n))

randomWalkOneDC :: Int -> Benchmark
randomWalkOneDC n = bench "oneDC" $ nfIO (evalRandIO (RW.oneDC n))

randomWalkTwoDC :: Int -> Benchmark
randomWalkTwoDC n = bench "twoDC" $ nfIO (evalRandIO (RW.twoDC n))

benchmarkRandomWalk :: [Benchmark]
benchmarkRandomWalk =
  [ randomWalkOneD 10000
  , randomWalkTwoD 10000
  , randomWalkOneDC 10000
  , randomWalkTwoDC 10000
  ]
