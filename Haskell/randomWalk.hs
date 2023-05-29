module RandomWalk (
    oneD,
    twoD,
    oneDC,
    twoDC,
    plot',
    plotavg
  ) where

import Control.Monad.Random
import Data.Text.Chart
import Data.List (transpose)

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' h fa fb = h <$> fa <*> fb

-- simple --

step :: RandomGen g => Rand g Int
step = getRandomR (0, 1 :: Int) >>= \x -> return (if x == 0 then -1 else 1)

-- one dimensional simple
oneD :: RandomGen g => Int -> Rand g [Int]
oneD n = liftA2' (scanl (+)) (step) (sequence (replicate n step))

-- one dimensional simple
twoD :: RandomGen g => Int -> Rand g [(Int, Int)]
twoD n = liftA2' (scanl add) (step2D) (sequence (replicate n step2D))
    where
        step2D :: RandomGen g => Rand g (Int, Int)
        step2D = liftA2' (,) (step) (step)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (u, v) = (x+u, y+v)

-- continuous --

stepC :: RandomGen g => Rand g Double
stepC = getRandomR (-1,1)

oneDC :: RandomGen g => Int -> Rand g [Double]
oneDC n = liftA2' (scanl (+)) (stepC) (sequence (replicate n stepC))

twoDC :: RandomGen g => Int -> Rand g [(Double, Double)]
twoDC n = liftA2' (scanl addC) (step2DC) (sequence (replicate n step2DC))
    where
        step2DC :: RandomGen g => Rand g (Double, Double)
        step2DC = liftA2' (,) (stepC) (stepC)

addC :: (Double, Double) -> (Double, Double) -> (Double, Double)
addC (x, y) (u, v) = (x+u, y+v)

-- graphing -- 

average :: [[Int]] -> [Double]
average lists = map av (transpose lists)
  where
    av xs = fromIntegral (sum xs) / fromIntegral (length xs)

plot' :: Int -> IO ()
plot' n = do
    g <- newStdGen
    let path = map (toInteger) (evalRand (oneD n) g)
    --print path
    plot path

plotavg :: Int -> Int -> IO ()
plotavg n k = do
    g <- newStdGen
    let paths = evalRand (sequence (replicate k (oneD n))) g
    --print (average paths)
    let vals = map (\x -> round (x*100)) (average paths)
    plot vals
