-- TestGlobber.hs
module Main (main) where

{- package dependencies -}
import Test.Hspec

{- locally defined dependencies -}
import Globber 

main :: IO ()
main = hspec $ do
	describe "Glob test" $ do
		describe "empty pattern" $ do
			it "matches empty string" $ 
				matchGlob "" "" `shouldBe` True
			it "shouldn't match non-empty string" $ 
				matchGlob "" "string" `shouldBe` False
		describe "question mark cases" $ do 
			it "matches any single character" $ 
				matchGlob "?" "*" `shouldBe` True
			it "matches any character within a longer string" $ 
				matchGlob "?s" "xs" `shouldBe` True
		describe "star cases" $ do
			it "matches any string" $
				matchGlob "*" "xs" `shouldBe` True
			it "matches empty string" $
				matchGlob "*" "" `shouldBe` True
		describe "escaped character cases" $ do
			it "matches that particular character" $ do
				matchGlob "\\s" "s" `shouldBe` True