module Util where

import System.Environment.Executable
import System.Directory
import System.FilePath

scriptPathToFilePath :: ScriptPath -> IO FilePath
scriptPathToFilePath (RunGHC fp) = return fp
scriptPathToFilePath (Executable fp) = return fp
scriptPathToFilePath Interactive = getCurrentDirectory

readInput :: IO String
readInput = do
  exePath <- getScriptPath >>= scriptPathToFilePath
  let inputPath = combine (takeDirectory exePath) "input.txt"
  readFile inputPath

eachCons :: Int -> [a] -> [[a]]
eachCons _ [] = []
eachCons n xs@(_:rest)
  | length xs == n = [window]
  | otherwise = window : eachCons n rest
  where window = take n xs

tuplify2 :: [a] -> (a, a)
tuplify2 [a, b] = (a, b)
tuplify2 _ = error "oops"
