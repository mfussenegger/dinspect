{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Plot (
  Mark,
  PlotOptions(..),
  plot
) where

import           Data.Aeson                             (Value (..))
import qualified Data.HashMap.Strict                    as M
import           Data.Maybe                             (catMaybes, fromMaybe)
import           Data.Scientific                        (toRealFloat)
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import qualified Graphics.Rendering.Chart.Backend.Cairo as C
import           Graphics.Rendering.Chart.Easy          ((.=))
import qualified Graphics.Rendering.Chart.Easy          as C
import           System.Directory                       (findExecutable)
import           System.IO.Temp                         (withSystemTempFile)
import           System.Process                         (callProcess)


-- $setup
-- >>> import Data.Aeson (decode, Value (..))
-- >>> :set -XOverloadedStrings

data Mark = Line
          | Bar
          | Point
          deriving (Show, Read)


data PlotOptions = PlotOptions
  { mark :: Mark
  , title :: Text
  , x :: Text
  , y :: Text
  , color :: Maybe Text
  , output :: Maybe FilePath
  }
  deriving (Show)


plot :: PlotOptions -> Value -> IO ()
plot PlotOptions{..} rows =
  case output of
    (Just filepath) -> createPlot filepath
    _               -> withSystemTempFile "inspect-plot" plotThenOpen
  where
    createPlot = plotToFile mark title x y color rows
    plotThenOpen tmpFile tmpHandle = do
      createPlot tmpFile
      openCmd <- fromMaybe "xdg-open" <$> findExecutable "open"
      callProcess openCmd [tmpFile]


-- | Get x, y points from the JSON
--
-- >>> let rows = fromMaybe Null $ decode "[{\"foo\": 10, \"bar\": 20}]"
-- >>> getValues "foo" "bar" rows
-- [(10.0,20.0)]
getValues :: Text -> Text -> Value -> [(Double, Double)]
getValues x y (Array arr) = parseXY arr
  where
    parseXY = catMaybes . V.toList . fmap tupleFromObj
    tupleFromObj (Object obj) = do
      xVal <- asNumber <$> M.lookup x obj
      yVal <- asNumber <$> M.lookup y obj
      (,) <$> xVal <*> yVal
    tupleFromObj _            = Nothing
    asNumber (Number n) = Just $ toRealFloat n
    asNumber _          = Nothing
getValues _ _ _           = []
    

plotToFile :: Mark
           -> Text
           -> Text
           -> Text
           -> Maybe Text
           -> Value
           -> FilePath
           -> IO ()
plotToFile mark title x y color rows output = C.toFile C.def output $ do
  C.layout_title .= T.unpack title
  C.plot (C.line xLabel [getValues x y rows])
  where
    xLabel = T.unpack x
