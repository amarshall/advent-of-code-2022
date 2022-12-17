#!/usr/bin/env runhaskell

{-# LANGUAGE DataKinds,NumericUnderscores #-}

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Control.Lens
import Data.List
import Data.List.Split
import Data.Geometry.Point
import Data.Maybe
import System.Environment.Executable
import System.Directory
import System.FilePath
import Text.Regex.PCRE

scriptPathToFilePath :: ScriptPath -> IO FilePath
scriptPathToFilePath (RunGHC fp) = return fp
scriptPathToFilePath (Executable fp) = return fp
scriptPathToFilePath Interactive = getCurrentDirectory

tuplify2 :: [a] -> (a, a)
tuplify2 [a, b] = (a, b)
tuplify2 _ = error "oops"

readInput = do
  exePath <- getScriptPath >>= scriptPathToFilePath
  let inputPath = combine (takeDirectory exePath) "input.txt"
  readFile inputPath

type Pos = Point 2 Int
type Sensor = (Pos, Int)

parseLine :: String -> (Pos, Pos)
parseLine input =
  match & head & tail & map (\s -> read s :: Int) & chunksOf 2 & map (tuplify2 >>> uncurry Point2) & tuplify2
  where
    match = (input =~ "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)") :: [[String]]

getXY :: Pos -> [Int]
getXY p = [p ^. xCoord, p ^. yCoord]

distance :: Pos -> Pos -> Int
distance a b = zipWith (-) (getXY a) (getXY b) & map abs & sum

toSensor :: (Pos, Pos) -> Sensor
toSensor (sensor, beacon) = (sensor, distance sensor beacon)

sortBy' :: Ord a => Ord b => (a -> b) -> [a] -> [a]
sortBy' f = sortBy (\a b -> compare (f a) (f b))

isOverlapping :: Ord a => (a, a) -> (a, a) -> Bool
isOverlapping (a1, a2) (b1, b2) = a1 <= b2 && a2 >= b1

combineRanges :: Num a => Ord a => (a, a) -> (a, a) -> [(a, a)]
combineRanges a@(a1, a2) b@(b1, b2) = if isOverlapping a b then [(min a1 b1, max a2 b2)] else [a, b]

dedupRanges :: [(Int, Int)] -> [(Int, Int)]
dedupRanges ranges =
  foldl (\rs r -> init rs ++ combineRanges (last rs) r) [r] rs
  where
    r:rs = sortBy' (\(a,b) -> (a,-b)) ranges

noBeaconRange :: Int -> Sensor -> Maybe (Int, Int)
noBeaconRange y (sensor, range) =
  if dx > 0 then Just (sx - dx, sx + dx) else Nothing
  where
    sx = sensor ^. xCoord
    sy = sensor ^. yCoord
    dx = range - abs (sy - y)

part1 :: Int -> [Sensor] -> Int
part1 targetY sensors =
  sensors & mapMaybe (noBeaconRange targetY) & dedupRanges & map (uncurry $ flip (-)) & sum

main = do
  input <- readInput
  let (sensorPoints, beacons) = input & lines & map parseLine & unzip
  let allPoints = sensorPoints ++ beacons
  let sensors = zipWith (curry toSensor) sensorPoints beacons
  putStrLn $ "part1:" ++ show (part1 2_000_000 sensors)
