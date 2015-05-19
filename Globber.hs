-- Globber.hs
module Globber (matchGlob) where


type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
matchGlob ("*") (ys) = True
matchGlob xs@(x:xs') (ys) = 
	case x of
		'\\' -> xs' == ys
		_   -> xs == ys
matchGlob xs ys = xs == ys


maybeHead :: String -> Maybe Char
maybeHead (x:_) = Just x
maybeHead [] = Nothing