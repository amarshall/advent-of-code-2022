#!/usr/bin/env runhaskell2

{-# LANGUAGE DataKinds #-}

import Control.Category ((>>>))
import Control.Lens
import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, last, replicate)
import Data.List
import Util

type Grid = [[Element]]
type Shape = [XY]
type XY = Point 2 Int
newtype Jet = Jet Char deriving (Eq, Show)

data Element = Air | Floor | Rock deriving (Eq)
instance Show Element where
  show Air = "."
  show Rock = "#"
  show Floor = "-"

-- We model the Grid upside down, so row 0 is the non-floor bottom and row 1... is “above”

worldWidth = 7
newPieceVerticalPadding = 3
shapes =
  [ [Point2 0 0, Point2 1 0, Point2 2 0, Point2 3 0]  -- shape horizontal line
  , [Point2 1 0, Point2 0 1, Point2 1 1, Point2 2 1, Point2 1 2] -- shape cross
  , [Point2 0 0, Point2 1 0, Point2 2 0, Point2 2 1, Point2 2 2] -- shape angle (reflected over Y)
  , [Point2 0 0, Point2 0 1, Point2 0 2, Point2 0 3] -- shape vertical line
  , [Point2 0 0, Point2 1 0, Point2 0 1, Point2 1 1] -- shape square
  ]

leftEdge :: Shape -> Int
leftEdge = minimum . xCoords

bottomEdge :: Shape -> Int
bottomEdge = minimum . yCoords

xCoords :: Shape -> [Int]
xCoords = map (^. xCoord)

yCoords :: Shape -> [Int]
yCoords = map (^. yCoord)

shapeToStart :: Grid -> Shape -> Shape
shapeToStart grid shape = map (.+^ vec) shape
  where
    vec = Vector2 (leftEdge shape) (bottomEdge shape) ^+^ Vector2 2 (length grid + newPieceVerticalPadding)

get :: Grid -> XY -> Maybe Element
get grid pos = grid ^. ix (pos ^. yCoord) ^? ix (pos ^. xCoord)

gridExpand :: Grid -> Shape -> Grid
gridExpand grid shape = grid ++ replicate (maximum (yCoords shape) - length grid + 1) (replicate worldWidth Air)

gridSet :: Grid -> XY -> Element -> Grid
gridSet grid pos val = grid & (ix (pos ^. yCoord) . ix (pos ^. xCoord)) .~ val

jetVector :: Jet -> Vector 2 Int
jetVector (Jet '>') = Vector2 1 0
jetVector (Jet '<') = Vector2 (-1) 0
jetVector _ = error "bad jet"

canMove :: Grid -> Shape -> Vector 2 Int -> Bool
canMove grid shape vec = minimum ys >= 0 && minimum xs >= 0 && maximum xs < worldWidth && all (get grid >>> (==) (Just Air)) newShape
  where
    newShape = map (.+^ vec) shape
    xs = xCoords newShape
    ys = yCoords newShape

moveShape :: Grid -> Jet -> Shape -> Shape
moveShape grid jet = applyMove (jetVector jet) >>> applyMove (Vector2 0 (-1))
  where applyMove vec shape = if canMove grid shape vec then map (.+^ vec) shape else shape

applyShape :: Grid -> Shape -> Grid
applyShape grid shape = grid & (`gridExpand` shape) & \grid -> foldr (\pos grid -> gridSet grid pos Rock) grid shape

dropShape :: (Grid, [Jet]) -> Shape -> (Grid, [Jet])
dropShape (grid, jets) shape = (applyShape (gridExpand' droppedShape) droppedShape, remainingJets)
  where
    gridExpand' = gridExpand grid
    shapeInit = shapeToStart grid shape
    (droppedShape, remainingJets) =
      (shapeInit, jets)
      & iterate (\(s, jet:jets) -> (moveShape (gridExpand' s) jet s, jets))
      & eachCons2
      & dropWhile (\((s1, _), (s2, _)) -> bottomEdge s1 /= bottomEdge s2)
      & head & snd

run :: [Jet] -> [Shape] -> Grid
run jets shapes = fst $ foldl' dropShape ([], jets) shapes

-- showGrid :: Grid -> String
-- showGrid = (++) [replicate worldWidth Floor] >>> reverse >>> map (concatMap show) >>> intercalate "\n"

part1 jets = length $ run (cycle jets) (take 2022 (cycle shapes))

main = do
  input <- readInput
  let jets = map Jet $ filter (/= '\n') input
  print $ part1 jets
