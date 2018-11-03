
{-# LANGUAGE OverloadedStrings #-}

module CliArgs
where

import           Options.Applicative
import           Plot                (PlotOptions (..))


plotOptions :: Parser PlotOptions
plotOptions =
  PlotOptions
  <$> option auto
    ( long "mark"
    <> metavar "MARK"
    <> help "Type of plot to use" )
  <*> strOption
    ( long "title"
    <> value ""
    <> metavar "TITLE" )
  <*> strOption
    ( long "x"
    <> metavar "X"
    <> help "X-Axis" )
  <*> strOption
    ( long "y"
    <> metavar "Y"
    <> help "Y-Axis" )
  <*> option auto
    ( long "color" 
    <> value Nothing
    <> help "Color field" )
  <*> option auto
    ( long "output"
    <> value Nothing
    <> help "Output file" )


commands = hsubparser
  ( command "plot" (info plotOptions (progDesc "Plot data")) )


parseArgs :: IO PlotOptions
parseArgs = execParser opts
  where
    opts = info (commands <**> helper)
      ( fullDesc <> progDesc "Inspect data" )
