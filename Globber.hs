-- Globber.hs
module Globber (matchGlob, matchLiteral) where


type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
matchGlob ("*") (_) = True
matchGlob xs@(x:xs') (ys) = 
	case x of
		'\\' -> xs' == ys
		_   -> xs == ys
matchGlob xs ys = xs == ys

matchLiteral :: GlobPattern -> String -> Bool
matchLiteral (x:xs') ys@(y:ys') 
	| (x == '\\' || x == '*' || x == '?' || x == '[') =
		matchLiteral xs' ys
	| otherwise = (x == y) && matchLiteral xs' ys'
matchLiteral [] []  = True
matchLiteral _ _ = False

