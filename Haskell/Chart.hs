{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- Modified from Data.Text.Chart

module Chart
    ( plot
    , plotWith
    , Options
    , options
    , height
    , minY
    , maxY
    , style
    , colors
    , getANSI
    , resetCode
    , makeLegend
    , contour
    , contourWith
    , contourF
    , contourFWith
    , colorPalette
    , monochromePalette
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
import Data.List               (unfoldr, dropWhileEnd, intercalate, transpose)
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

colorPalette :: [String]
colorPalette =
    [ "\ESC[0m \ESC[0m", "\ESC[0m \ESC[0m", "\ESC[0m \ESC[0m", "\ESC[0m \ESC[0m"
    , "\ESC[94m.\ESC[0m", "\ESC[94m.\ESC[0m", "\ESC[94m.\ESC[0m", "\ESC[94m.\ESC[0m"
    , "\ESC[94m-\ESC[0m", "\ESC[94m-\ESC[0m", "\ESC[94m-\ESC[0m"
    , "\ESC[96m,\ESC[0m", "\ESC[96m,\ESC[0m", "\ESC[96m,\ESC[0m"
    , "\ESC[96m'\ESC[0m", "\ESC[96m'\ESC[0m", "\ESC[96m'\ESC[0m"
    , "\ESC[96m-\ESC[0m", "\ESC[96m-\ESC[0m"
    , "\ESC[92m:\ESC[0m"
    , "\ESC[92m=\ESC[0m"
    , "\ESC[92m;\ESC[0m"
    , "\ESC[93mo\ESC[0m"
    , "\ESC[93m!\ESC[0m"
    , "\ESC[93m?\ESC[0m"
    , "\ESC[91mX\ESC[0m"
    , "\ESC[91m0\ESC[0m"
    , "\ESC[91m8\ESC[0m"
    , "\ESC[95m#\ESC[0m"
    , "\ESC[95m%\ESC[0m"
    , "\ESC[95m&\ESC[0m"
    , "\ESC[97m$\ESC[0m"
    , "\ESC[97m@\ESC[0m"
    ]

monochromePalette :: [String]
monochromePalette = map (:[]) "       ....---,,,'''--:=;o!?X08#%&$@"

-- | plots a contour given a grid of z-values, and a palette gradient of low to high values
contourWith :: [String] -> [[Double]] -> IO ()
contourWith palette g = putStr $ to_ascii palette g

-- | contourWith default palette
contour :: [[Double]] -> IO ()
contour g = contourWith colorPalette g

-- | plots a contour of a given function z(x, y)
--   with the x range (xt, xb), y range (yt, yb)
--   and with step size w in the x, h in the y direction
contourFWith :: [String] -> (Int, Int) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double) -> IO ()
contourFWith palette (w,h) (xt,xb) (yt,yb) z = putStr $ to_ascii palette $ grid (w,h) (xt,xb) (yt,yb) z

-- | contourFWith default palette
contourF :: (Int, Int) -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double) -> IO ()
contourF (w,h) (xt,xb) (yt,yb) z = contourFWith colorPalette (w,h) (xt,xb) (yt,yb) z

grid :: (Int,Int) -> (Double,Double) -> (Double,Double) -> (Double -> Double -> Double) -> [[Double]]
grid (w,h) (xt,xb) (yt,yb) z =
    [ [ z x y | x <- [xt, xt+stepx .. xb] ] | y <- [yt, yt+stepy .. yb] ]
    where stepx = (xb - xt) / (fromIntegral w)
          stepy = (yb - yt) / (fromIntegral h)

to_ascii :: [String] -> [[Double]] -> String
to_ascii palette grid = unlines $ reverse $ map (\row -> concatMap (\val -> valToChar val palette) row) grid
    where 
        valToChar :: Double -> [String] -> String
        valToChar val palette =
            let numChars = length palette
                minG = minimum (concat grid)  -- Minimum value in the entire grid
                maxG = maximum (concat grid)  -- Maximum value in the entire grid
                range = maxG - minG
                index = truncate ((val - minG) / range * fromIntegral (numChars - 1))
            in palette !! index


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

makeLegend :: [([Char], Maybe String)] -> [Char]
makeLegend key = 
  let strs = map fst key
      horizontalLine = "\x1b[12C+" ++ replicate (maximum (map length strs) + 2) '-' ++ "+"
      colorStrs = map (\k -> (getANSI (snd k)) ++ fst k ++ resetCode) key
      paddedStrings = map (\s -> "\x1b[12C| " ++ s ++ replicate (maximum (map length colorStrs) - length s + 1) ' ' ++ "|") colorStrs
  in unlines ( horizontalLine : paddedStrings ++ [horizontalLine])

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
