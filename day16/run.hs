#!/usr/bin/env runhaskell

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Category ((>>>))
import Control.Lens
import Control.Parallel.Strategies
import Data.Coerce
import Data.Graph
import Data.List
import Data.Maybe
import Data.Tuple.Extra
import System.Environment.Executable
import System.Directory
import System.FilePath
import Text.Regex.PCRE

scriptPathToFilePath :: ScriptPath -> IO FilePath
scriptPathToFilePath (RunGHC fp) = return fp
scriptPathToFilePath (Executable fp) = return fp
scriptPathToFilePath Interactive = getCurrentDirectory

eachCons :: Int -> [a] -> [[a]]
eachCons _ [] = []
eachCons n xs@(_:rest)
  | length xs == n = [window]
  | otherwise = window : eachCons n rest
  where window = take n xs

tuplify2 :: [a] -> (a, a)
tuplify2 [a, b] = (a, b)
tuplify2 _ = error "oops"

readInput = do
  exePath <- getScriptPath >>= scriptPathToFilePath
  let inputPath = combine (takeDirectory exePath) "input.txt"
  readFile inputPath

parseLine :: String -> Valve
parseLine input = (name, Pressure (read rate :: Int), adjs)
  where
    match = (input =~ "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z, ]+)") :: [[String]]
    name:rate:adjStr = match & head & tail
    adjs = (head adjStr =~ "([A-Z]+)" :: [[String]]) & map head

type ValveName = String
type Valve = (ValveName, Pressure, [ValveName])
type Node = (Pressure, ValveName, [ValveName])

newtype Time = Time Int deriving (Eq, Num, Show, Ord)
newtype Pressure = Pressure Int deriving (Eq, Num, Show, Ord)

maxTime = Time 30

walk :: Graph
     -> (Vertex -> Node)
     -> (String -> Maybe Int)
     -> Int -- vertex
     -> [Int] -- unOpenedVertexes
     -> [Int] -- onlyWalkedVertexes
     -> Time
     -> Pressure
     -> Pressure
walk graph nodeFromVertex vertexFromKey vertex unOpenedVertexes onlyWalkedVertexes time pressure =
  if time >= maxTime || null unOpenedVertexes
     then pressure
     else maximum $ runEval $ parList rseq $ pressure : onlyWalk ++ (if vertex `elem` unOpenedVertexes then openAndWalk else [])
    where
      walk' = walk graph nodeFromVertex vertexFromKey
      openAndWalk = map (\nextVertex -> walk' nextVertex (unOpenedVertexes \\ [vertex]) [] (time + 2) (pressure + openedPressure)) nexts
      onlyWalk = map (\nextVertex -> walk' nextVertex unOpenedVertexes (vertex:onlyWalkedVertexes) (time + 1) pressure) (nexts \\ onlyWalkedVertexes)
      (thisRate, name, adjs) = nodeFromVertex vertex
      nexts = map (vertexFromKey >>> fromJust) adjs
      openedPressure = thisRate * coerce (max 0 (maxTime - time - 1))

main = do
  input <- readInput
  let valves = input & lines & map parseLine
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ map (\(name, rate, adjs) -> (rate, name, adjs)) valves
  let unOpenedVertexes = valves & filter (\(_, rate, _) -> rate > 0) & map (fst3 >>> vertexFromKey >>> fromJust)
  print $ walk graph nodeFromVertex vertexFromKey (fromJust $ vertexFromKey "AA") unOpenedVertexes [] (Time 0) (Pressure 0)
