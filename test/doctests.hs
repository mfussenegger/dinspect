
import Test.DocTest

main = doctest
  [ "-isrc"
  , "src/Main.hs"
  , "src/CliArgs.hs"
  , "src/Plot.hs"
  ]
