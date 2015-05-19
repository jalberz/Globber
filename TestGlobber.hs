{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-19
-}

-- TestGlobber.hs
module Main (main) where

{- package dependencies -}
import Test.Hspec

{- locally defined dependencies -}
import Globber 

main :: IO ()
main = hspec $ do
	describe "Glob tests" $ do
		--Part 1 : Glob Matching
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
		--Part 2 : Literal Matching
		describe "literal matching cases" $ do
			it "treats all non-special characters as literal" $ do
				matchLiteral "\\*?[s" "s" `shouldBe` True
		--Part 3 : Literal Escaping
		describe "literal escape cases" $ do
			it "escapes backslashes" $ do
				matchLiteralEscape "\\[a]" "[a]" `shouldBe` True
			it "elides multiple backslashes" $ do
				matchLiteralEscape "\\*\\*\\?" "**?" `shouldBe` True
			it "collapses layers of backslashes" $ do
				matchLiteralEscape "\\\\a\\\\" "\\a\\" `shouldBe` True
			it "elides interior backslashes" $ do
				matchLiteralEscape "ab\\*ba" "ab*ba" `shouldBe` True
			it "collapses brackets" $ do
				matchLiteralEscape "ab[a\\]]ba" "ababa" `shouldBe` True
			it "can collapse brackets another way" $ do
				matchLiteralEscape "ab[a\\]]ba" "aba]ba" `shouldBe` True
