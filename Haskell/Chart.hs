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
    , color
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
import Data.List               (unfoldr, dropWhileEnd)
import Text.Printf             (printf)
import Data.Bool               (bool)

data Options =
  Options { height :: Double               -- ^ Allows setting the height of the chart.
          , minY   :: Maybe Double         -- ^ Optional minimum Y value.
          , maxY   :: Maybe Double         -- ^ Optional maximum Y value.
          , style  :: String               -- ^ Style of the plot: "line" or "points".
          , color  :: Maybe String         -- ^ Color of the plot: "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", or 'Nothing' for default color.
          }

-- | Provides default options: @Options { height = 14.0, minY = Nothing, maxY = Nothing, style = "line", color = Nothing }@.
options :: Options
options =
  Options { height = 14.0
          , minY = Nothing
          , maxY = Nothing
          , style = "line"
          , color = Nothing
          }

newArray2D :: Integer -> Integer ->
              ST s (STArray s (Integer, Integer) String)
newArray2D dimX dimY = newArray ((0,0), (dimX, dimY)) " "

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

pad :: [Double] -> Int
pad series =
  let floats = series
      toStr :: [Double] -> [String]
      toStr = fmap (printf "%0.2f")
  in  maximum $ length <$> toStr floats

ansi :: Maybe String -> String
ansi (Just "black")   = "30"
ansi (Just "red")     = "91"
ansi (Just "green")   = "92"
ansi (Just "yellow")  = "93"
ansi (Just "blue")    = "94"
ansi (Just "magenta") = "95"
ansi (Just "cyan")    = "96"
ansi (Just "white")   = "97"
ansi (Just "reset")   = "0"
ansi _                = "" 
getANSI :: Maybe String -> String
getANSI (Just c) = "\x1b[" ++ ansi (Just c) ++ "m"
getANSI Nothing  = ""

resetCode = getANSI (Just "reset")

plotWith' :: Options -> [Double] -> [String]
plotWith' opts series =
    -- variables and functions
    let min' = minimum series
        max' = maximum series
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
        width = toInteger $ length series + 3
        usePoints = style opts == "points" -- Check if style is "points"
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
                printf ("%" ++ show (pad series) ++ ".2f") label ++ 
                getANSI (color opts)
            result [round $ y - min2] [offset - 1] $ 
                resetCode ++
                bool "┤" "┼" (y == 0) ++
                getANSI (color opts)

        -- initial value
        let first = head series * ratio - min2
        result [round $ fromInteger rows - first] [offset - 1] $ 
            resetCode ++ "┼" ++ getANSI (color opts)

        -- plot based on style
        if usePoints
            then do -- Plot points
                forM_ [0..length series - 1] $ \x -> do
                    let offset' = toInteger x + offset
                    let y = round (series !! x * ratio) - round min2
                    result [rows - y] [offset'] "*"
            else do -- Plot line
                forM_ [0..length series - 2] $ \x -> do
                    let offset' = toInteger x + offset
                    let y' i = round (series !! i * ratio) - round min2
                    let (y0, y1) = (y' x, y' (x + 1))
                    if y0 == y1 then
                        result [rows - y0] [offset'] "─"
                    else do
                        result [rows - y1] [offset'] $
                            getANSI (color opts) ++
                            bool "╭" "╰" (y0 > y1)
                        result [rows - y0] [offset'] $
                            getANSI (color opts) ++
                            bool "╯" "╮" (y0 > y1)

                        forM_ [min y0 y1 + 1..max y0 y1 - 1] $ \y ->
                            result [rows - y] [offset'] "│"

        elems <- getElems arr
        return $ elems ++ [resetCode] ++ ["\x1b[A"]

-- | Takes a list of Doubles and prints out a corresponding chart
--   with a default terminal height of 14 blocks.
plot :: [Double] -> IO ()
plot x = if length x < 1 then return () else plotWith options x

-- | Same as plot but it's possible to define custom options.
--   Example: @'plotWith' options { 'height' = 20.0, 'minY' = Just 0.0, 'maxY' = Just 100.0, 'color' = Just "red" }@
plotWith :: Options -> [Double] -> IO ()
plotWith options' series = forM_ result $
      putStrLn . dropWhileEnd isSpace . concat
    where result = splitEvery (length series + 4) $ plotWith' options' series
