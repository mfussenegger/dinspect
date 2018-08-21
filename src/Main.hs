{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text                              as T
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Options.Applicative


data PlotMode = Lines
  deriving (Show, Read)


data PlotOptions = PlotOptions
  { mode :: PlotMode
  , verbose :: Bool
  , title :: T.Text
  }
  deriving (Show)


plotOptions :: Parser PlotOptions
plotOptions =
  PlotOptions
  <$> option auto
    ( long "mode"
    <> metavar "MODE"
    <> help "Type of plot to use" )
  <*> switch
    ( long "verbose"
    <> help "Enable verbose output" )
  <*> strOption
    ( long "title"
    <> metavar "TITLE"
    <> value ""
    <> help "Title of the plot" )


commands = hsubparser
  ( command "plot" (info plotOptions (progDesc "Plot data")) )


doPlot :: PlotMode -> T.Text -> IO ()
doPlot mode title = toFile def "dummy.png" $ do
  layout_title .= T.unpack title
  plot (line "1" [values])
  plot (line "2" [fmap (\(x, y) -> (x, y + 10)) values])
  where
    values :: [(Double, Double)]
    values = [(1, 1), (2, 2), (3, 3), (4, 4)]


main :: IO ()
main = do
  command <- execParser opts
  case command of
    PlotOptions{..} -> doPlot mode title
  where
    opts = info (commands <**> helper)
      ( fullDesc <> progDesc "Inspect data" )
