{-# LANGUAGE RecordWildCards #-}

module Main where

import           CliArgs              (parseArgs)
import           Data.Aeson           (Value (..), eitherDecode)
import           Data.ByteString.Lazy as BL
import           Plot                 (Mark, PlotOptions (..), plot)

main :: IO ()
main = do
  rows <- (eitherDecode <$> BL.getContents) :: IO (Either String Value)
  case rows of
    (Right rows') -> do
      command <- parseArgs
      case command of
        opts@PlotOptions{..} -> plot opts rows'
    (Left errorMsg) -> error errorMsg
