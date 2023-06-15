{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- Modified from Data.Text.Chart

module Chart
    ( -- * Plot
      plot
    , plotWith
      -- * Options
    , Options
    , options
    , height
    , minY
    , maxY
    , style
    , colors
      -- * Helpers
    , getANSI
    , resetCode
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative     ((<$>))
import Control.Monad.ST.Safe   (ST, runST)
#else
import Control.Monad.ST        (ST, runST)
#endif
import Control.Monad           (forM_)
import Data.Array.ST.Safe      (STArray, getElems, writeArray, newArray)
import Data.Char               (isSpace)
import Data.List               (unfoldr, dropWhileEnd, intercalate)
import Text.Printf             (printf)
import Data.Bool               (bool)

data Options =
  Options { height :: Double               -- ^ Height of the chart.
          , minY   :: Maybe Double         -- ^ Minimum Y value.
          , maxY   :: Maybe Double         -- ^ Maximum Y value.
          , style  :: String               -- ^ Style of the plot: "line" or "points".
          , colors :: [Maybe String]       -- ^ Colors of the plot: Just "black",
                                           --                       Just "red", 
                                           --                       Just "green", 
                                           --                       Just "yellow",
                                           --                       Just "blue",
                                           --                       Just "magenta",
                                           --                       Just "cyan",
                                           --                       Just "white", 
                                           --                       'Nothing' for default terminal settings.
          }

-- | Provides default options
options :: Options
options =
  Options { height = 14.0
          , minY = Nothing
          , maxY = Nothing
          , style = "line"
          , colors = [Nothing, Just "blue", Just "red", Just "green", Just "magenta", Just "yellow", Just "cyan"]
          }

newArray2D :: Integer -> Integer ->
              ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: [[Double]] -> Int
pad series =
  let floats = concat series
      toStr :: [Double] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

ansi :: Maybe String -> String
ansi color = case color of
    Just "black"   -> "30"
    Just "red"     -> "91"
    Just "green"   -> "92"
    Just "yellow"  -> "93"
    Just "blue"    -> "94"
    Just "magenta" -> "95"
    Just "cyan"    -> "96"
    Just "white"   -> "97"
    Just "reset"   -> "0"
    _              -> ""

getANSI :: Maybe String -> String
getANSI colors = "\x1b[" ++ ansi colors ++ "m"

resetCode = getANSI (Just "reset")

plotWith' :: Options -> [[Double]] -> [String]
plotWith' opts series =
    -- variables and functions
    let min' = minimum (concat series)
        max' = maximum (concat series)
        range = abs $ max' - min'
        offset = 3
        ratio = if range == 0.0 then 1.0 else height opts / range
        min2 = case minY opts of
                  Just y -> y * ratio
                  Nothing -> min' * ratio
        max2 = case maxY opts of
                  Just y -> y * ratio
                  Nothing -> max' * ratio
        rows = round $ abs $ max2 - min2
        width = toInteger $ length (concat series) + 3
        usePoints = style opts == "points" -- Check if style is "points"
        useLine = style opts == "line"     -- Check if style is "line"
        colorList = cycle (colors opts)    -- Cycle through the colors
    in runST $ do
        -- array creation
        arr <- newArray2D rows width
        let result x y = writeArray arr (head x, head y)

        -- axis and labels
        forM_ [min2..max2] $ \y -> do
            let label = if rows == 0 then y
                        else max' - (y - min2) * range / fromIntegral rows
            result [round $ y - min2] [max 0 $ offset - 5] $
                resetCode ++
                printf ("%" ++ show (pad series) ++ ".2f") label
            result [round $ y - min2] [offset - 1] $
                resetCode ++
                bool "┤" "┼" (y == 0)

        -- initial value
        let first = head (head series) * ratio - min2
        result [round $ fromInteger rows - first] [offset - 1] $
            resetCode ++ "┼"

        -- plot based on style
        if usePoints
            then do -- Plot points
                forM_ [0..length (head series) - 1] $ \x -> do
                    let offset' = toInteger x + offset
                    forM_ (zip [0..] series) $ \(i, s) -> do
                        let y = round (s !! x * ratio) - round min2
                        result [rows - y] [offset'] $ getANSI (colorList !! i) ++ "*"
            else do -- Plot line
                forM_ [0..length (head series) - 2] $ \x -> do
                    let offset' = toInteger x + offset
                    forM_ (zip [0..] series) $ \(i, s) -> do 
                        let y' j = round (s !! j * ratio) - round min2
                        let (y0, y1) = (y' x, y' (x + 1))
                        if y0 == y1 then
                            result [rows - y0] [offset'] $
                            getANSI (colorList !! i) ++ "─"
                        else do
                            result [rows - y1] [offset'] $
                                getANSI (colorList !! i) ++
                                bool "╭" "╰" (y0 > y1)
                            result [rows - y0] [offset'] $
                                getANSI (colorList !! i) ++
                                bool "╯" "╮" (y0 > y1)
                            forM_ [min y0 y1 + 1..max y0 y1 - 1] $ \y ->
                                result [rows - y] [offset'] $ 
                                getANSI (colorList !! i) ++ "│"

        elems <- getElems arr
        return $ elems ++ [resetCode] ++ ["\x1b[A"]

-- | Takes a list of lists of Doubles and renders them as a chart.
-- Each inner list represents a separate series, and each Double is a data point in that series.
plotWith :: Options -> [[Double]] -> IO ()
plotWith options' series = forM_ result $
      putStrLn . dropWhileEnd isSpace . concat
    where result = splitEvery (length (concat series) + 4) $ plotWith' options' series

-- | plotWith using default options.
plot :: [[Double]] -> IO ()
plot = plotWith options
