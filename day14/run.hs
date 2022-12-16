#!/usr/bin/env runhaskell

{-# LANGUAGE DataKinds #-}

import Control.Category ((>>>))
import Control.Lens hiding (re)
import Data.Function
import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, last)
import Data.List
import Data.List.Extra (concatUnzip)
import Data.List.Split
import Data.Matrix
import Data.Maybe
import Prelude
import System.Environment.Executable
import System.Directory
import System.FilePath
import Text.RE.Replace
import Text.RE.PCRE.String

scriptPathToFilePath :: ScriptPath -> IO FilePath
scriptPathToFilePath (RunGHC fp) = return fp
scriptPathToFilePath (Executable fp) = return fp
scriptPathToFilePath Interactive = getCurrentDirectory

readInput = do
  exePath <- getScriptPath >>= scriptPathToFilePath
  let inputPath = combine (takeDirectory exePath) "input.txt"
  readFile inputPath

tuplify2 :: [a] -> (a, a)
tuplify2 [a, b] = (a, b)
tuplify2 _ = error "oops"

upUntil :: (a -> Bool) -> (a -> a) -> a -> a
upUntil p f x
  | p (f x) = x
  | otherwise = upUntil p f (f x)

eachCons :: Int -> [a] -> [[a]]
eachCons _ [] = []
eachCons n xs@(_:rest)
  | length xs == n = [window]
  | otherwise = window : eachCons n rest
  where window = take n xs

type Pos = Point 2 Int
data Cell = Air | Rock | Origin | Sand deriving (Eq)
type Grid = Matrix Cell
type PLine = Line 2 Int

instance Show Cell where
  show Air = "."
  show Rock = "#"
  show Origin = "+"
  show Sand = "o"

center = 500

pointForMatrix :: Pos -> (Int, Int)
pointForMatrix p = (p ^. yCoord, p ^. xCoord)

safeGetP :: Pos -> Matrix a -> Maybe a
safeGetP = pointForMatrix >>> uncurry safeGet

setElemP :: a -> Pos -> Matrix a -> Matrix a
setElemP v = pointForMatrix >>> Data.Matrix.setElem v

safeSetP :: a -> Pos -> Matrix a -> Maybe (Matrix a)
safeSetP v = pointForMatrix >>> safeSet v

readInt s = read s :: Int

dimensions :: [Pos] -> Pos
dimensions points = let
  get reducer getter = points & map (^. getter) & foldr1 reducer
  x = get max xCoord - get min xCoord + 1
  y = get max yCoord + 1
  in Point2 x y

getOffset :: [Pos] -> Vector 2 Int
getOffset points = let
  get reducer getter = points & map (^. getter) & foldr1 reducer
  x = - get min xCoord + 1
  in Vector2 x 1

pointsToLines :: [Pos] -> [PLine]
pointsToLines = eachCons 2 >>> map (tuplify2 >>> uncurry lineThrough)

parse :: String -> [[Pos]]
parse input = input & lines & map (splitOn " -> " >>> map parsePos)
  where parsePos = splitOn "," >>> map readInt >>> tuplify2 >>> uncurry Point2

mkGrid :: [Pos] -> Grid
mkGrid points = uncurry matrix (points & dimensions & pointForMatrix) $ const Air

lineToPoints :: PLine -> [Pos]
lineToPoints line = map (start .+^) offsets
  where
    start = line ^. anchorPoint
    dx = line ^. (direction . xComponent)
    dy = line ^. (direction . yComponent)
    xs = extend $ if dx >= 0 then [0..dx] else [dx..0]
    ys = extend $ if dy >= 0 then [0..dy] else [dy..0]
    extend as = if as == [0] then repeat 0 else as
    offsets = zipWith Vector2 xs ys

addRockLine :: PLine -> Grid -> Grid
addRockLine line grid = foldr (setElemP Rock) grid (lineToPoints line)

floorPoints :: [Pos] -> [Pos]
floorPoints rawPoints = lineThrough (Point2 startX (height - 2)) (Point2 endX (height - 2)) & lineToPoints
  where
    floorOffset = 3
    startX = center - (height `div` 2) - floorOffset - 100 -- random number that is big enough...
    endX = center + (height `div` 2) + floorOffset + 100
    height = (dimensions rawPoints ^. yCoord) + floorOffset

getNext :: [Maybe Cell] -> Grid -> Pos -> Maybe Pos
getNext extraAvailable grid point =
  find isAvailable [pointBelow, pointLeft, pointRight]
  where
    isAvailable p = safeGetP p grid `elem` Just Air : extraAvailable
    pointBelow = point .+^ Vector2 0 1
    pointLeft = point .+^ Vector2 (-1) 1
    pointRight = point .+^ Vector2 1 1

simulate :: [Maybe Cell] -> Pos -> Grid -> Maybe Grid
simulate extraAvailable point grid =
  case next of
    Just point -> safeGetP point grid >> simulate extraAvailable point grid
    Nothing -> safeSetP Sand point grid
  where next = getNext extraAvailable grid point

mk :: [[Pos]] -> (Grid, Pos)
mk pointsRaw = (grid, sandOrigin)
  where
    offset = getOffset $ concat pointsRaw
    points = map (map (.+^ offset)) pointsRaw
    lines = concatMap pointsToLines points
    sandOrigin = Point2 center 0 .+^ offset
    grid = concat pointsRaw & mkGrid & \g -> foldr addRockLine g lines & setElemP Origin sandOrigin

countSand :: Grid -> Int
countSand = toList >>> filter (== Sand) >>> length

part1 pointsRaw = do
  let (gridStart, sandOrigin) = mk pointsRaw
  let grid = fromJust $ upUntil isNothing (simulate [Nothing] sandOrigin . fromJust) (Just gridStart)
  putStrLn $ "part1:" ++ show (countSand grid)

part2 pointsRaw1 = do
  let pointsRaw = pointsRaw1 ++ [floorPoints (concat pointsRaw1)]
  let (gridStart, sandOrigin) = mk pointsRaw
  let grid = fromJust $ until (\g -> safeGetP sandOrigin (fromJust g) == Just Sand) (simulate [] sandOrigin . fromJust) (Just gridStart)
  putStrLn $ "part2:" ++ show (countSand grid)

main = do
  input <- readInput
  let rawPoints = parse input
  part1 rawPoints
  part2 rawPoints
