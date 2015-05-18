-- TestGlobber.hs
module Main (main) where

{- package dependencies -}
import Test.Hspec

{- locally defined dependencies -}
import Globber 

import Distribution.Simple

main :: IO ()
  main = hspec $
    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True