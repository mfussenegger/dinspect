{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Maybe                             (fromMaybe)
import           Data.Text                              as T
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative
import           System.Directory                       (findExecutable)
import           System.IO.Temp                         (withSystemTempFile)
import           System.Process                         (callProcess)


data PlotMode = Lines
  deriving (Show, Read)


data PlotOptions = PlotOptions
  { mode :: PlotMode
  , title :: T.Text
  , output :: Maybe FilePath
  }
  deriving (Show)


plotOptions :: Parser PlotOptions
plotOptions =
  PlotOptions
  <$> option auto
    ( long "mode"
    <> metavar "MODE"
    <> help "Type of plot to use" )
  <*> strOption
    ( long "title"
    <> metavar "TITLE"
    <> value ""
    <> help "Title of the plot" )
  <*> option auto
    ( long "output"
    <> value Nothing
    <> help "Output file" )


commands = hsubparser
  ( command "plot" (info plotOptions (progDesc "Plot data")) )


plotToFile :: PlotMode -> T.Text -> FilePath -> IO ()
plotToFile mode title output = toFile def output $ do
  layout_title .= T.unpack title
  plot (line "1" [values])
  plot (line "2" [fmap (\(x, y) -> (x, y + 10)) values])
  where
    values :: [(Double, Double)]
    values = [(1, 1), (2, 2), (3, 3), (4, 4)]


doPlot :: PlotMode -> T.Text -> Maybe FilePath -> IO ()
doPlot mode title mOutput =
  case mOutput of
    (Just output) -> plotToFile mode title output
    _             -> withSystemTempFile "inspect-plot" plotThenOpen
  where
    plotThenOpen tmpFile tmpHandle = do
      plotToFile mode title tmpFile
      openCmd <- fromMaybe "xdg-open" <$> findExecutable "open"
      callProcess openCmd [tmpFile]


main :: IO ()
main = do
  command <- execParser opts
  case command of
    PlotOptions{..} -> doPlot mode title output
  where
    opts = info (commands <**> helper)
      ( fullDesc <> progDesc "Inspect data" )
