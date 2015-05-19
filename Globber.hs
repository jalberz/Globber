-- Globber.hs
module Globber (matchGlob, matchLiteral, matchLiteralEscape) where


type GlobPattern = String


--Part 1
matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
--accept anything for '*'
matchGlob ("*") (_) = True
--check for '\', elide it
matchGlob ('\\':xs') (ys) = xs' == ys
matchGlob xs ys = xs == ys

--Part 2
matchLiteral :: GlobPattern -> String -> Bool
matchLiteral (x:xs') ys@(y:ys')
	--catch special symbols
	| (x == '\\' || x == '*' || x == '?' || x == '[') =
		--elide them
		matchLiteral xs' ys
		--otherwise continue matching
	| otherwise = (x == y) && matchLiteral xs' ys'
matchLiteral [] []  = True
matchLiteral _ _ = False

--Part 3
matchLiteralEscape :: GlobPattern -> String -> Bool
matchLiteralEscape xs@(x:xs') ys@(_:ys')
	--check for special symbols
	| (x == '\\' || x == '*' || x == '?' || x == '[' || x == ']') =
		--test overall strings
		xs == ys
		--remove special and other front char and check
		|| xs' == ys'
		--check remainder with entirety of comparison list
		|| matchLiteralEscape xs' ys
		--check remainder with tail of comparison list
		|| matchLiteralEscape xs' ys'
	|otherwise =
		-- check for overall match and continue
		xs == ys || matchLiteralEscape xs' ys'
matchLiteralEscape [] [] = True
matchLiteralEscape _ _ = False


{-
Test output:
reprocessing library globber-0.0.1...
[1 of 1] Compiling Globber          ( Globber.hs, dist/build/Globber.o )
In-place registering globber-0.0.1...
Preprocessing test suite 'test-globber' for globber-0.0.1...
[1 of 2] Compiling Globber          ( Globber.hs, dist/build/test-globber/test-globber-tmp/Globber.o )
Linking dist/build/test-globber/test-globber ...
Running 1 test suites...
Test suite test-globber: RUNNING...
Test suite test-globber: PASS
Test suite logged to: dist/test/globber-0.0.1-test-globber.log
1 of 1 test suites (1 of 1 test cases) passed.
-}

