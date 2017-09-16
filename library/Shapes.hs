module Shapes where

import Shape
       (Coord2D, Intersection(Intersection, runIntersection),
        Shape(Shape), outside)
import Utils (diameter, euclidianDistance)

type Height = Double

type Width = Double

data Fill
  = Filled
  | Hollow

data Origin
  = TopLeft Coord2D
  | Center Coord2D

originToTopLeft :: Origin -> Width -> Height -> Coord2D
originToTopLeft (Center (x, y)) width height =
  (x - (width / 2), y - (height / 2))
originToTopLeft (TopLeft coord) _ _ = coord

originToCenter :: Origin -> Width -> Height -> Coord2D
originToCenter (Center coord) _ _ = coord
originToCenter (TopLeft (x, y)) width height =
  (x + (width / 2), y + (height / 2))

circle :: Double -> Origin -> Shape Coord2D
circle radius origin =
  Shape $ \coord ->
    euclidianDistance
      (originToCenter origin (diameter radius) (diameter radius))
      coord <=
    radius

torus :: Double -> Double -> Origin -> Shape Coord2D
torus smallRadius bigRadius origin =
  runIntersection $
  Intersection (circle bigRadius origin) `mappend`
  Intersection (outside $ circle smallRadius origin)

rectangle :: Width -> Height -> Origin -> Fill -> Shape Coord2D
rectangle width height origin fill =
  let (x0, y0) = originToTopLeft origin width height
      withinX x = x >= x0 && x <= x0 + width
      withinY y = y >= y0 && y <= y0 + height
  in Shape $ \(x, y) ->
       case fill of
         Filled -> withinX x && withinY y
         Hollow ->
           (withinX x && y `elem` [y0, y0 + height]) ||
           (withinY y && x `elem` [x0, x0 + width])

square :: Width -> Origin -> Fill -> Shape Coord2D
square width = rectangle width width

point :: Origin -> Shape Coord2D
point = circle 0

-- https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
triangle :: Coord2D -> Coord2D -> Coord2D -> Shape Coord2D
triangle vertex1 vertex2 vertex3 =
  Shape $ \coord ->
    let b1 = sign coord vertex1 vertex2 < 0
        b2 = sign coord vertex2 vertex3 < 0
        b3 = sign coord vertex3 vertex1 < 0
    in b1 == b2 && b2 == b3
  where
    sign (x1, y1) (x2, y2) (x3, y3) =
      ((x1 - x3) * (y2 - y3)) - ((x2 - x3) * (y1 - y3))
{-
-- http://jsfiddle.net/PerroAZUL/zdaY8/1
triangle :: Coord2D -> Coord2D -> Coord2D -> Shape Coord2D
triangle (x0, y0) (x1, y1) (x2, y2) =
  Shape $ \(x, y) ->
    let area = (1 / 2) * (y0 * (x2 - x1) - y1 * x2 + x0 * (y1 - y2) + x1 * y2)
        sign =
          if area < 0
            then -1
            else 1
        s = (y0 * x2 - x0 * y2 + (y2 - y0) * x + (x0 - x2) * y) * sign
        t = (x0 * y1 - y0 * x1 + (y0 - y1) * x + (x1 - x0) * y) * sign
    in s > 0 && t > 0 && s + t < 2 * area * sign
-}
