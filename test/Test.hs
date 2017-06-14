module Main where

import Test.Hspec
import qualified STLC as STLC
import qualified SpecDiscovery as S

main :: IO ()
main = hspec S.spec
