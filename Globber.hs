-- Globber.hs
module Globber (matchGlob) where

import Distribution.Simple 

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = True