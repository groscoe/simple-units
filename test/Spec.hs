import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Units/Simple.hs"
  , "src/Units/Simple/Arithmetic.hs"
  , "src/Units/Simple/Quantity.hs"
  , "src/Units/Simple/Unit.hs"
  ]
