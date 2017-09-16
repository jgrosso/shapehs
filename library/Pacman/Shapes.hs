module Pacman.Shapes where

import Shape
       (Coord2D, Intersection(Intersection, runIntersection), Shape,
        outside)
import Shapes (Origin, circle, originToCenter, triangle)

pacman :: Origin -> Shape Coord2D
pacman origin =
  let (x, y) = originToCenter origin diameter diameter
  in runIntersection $
     Intersection (circle radius origin) `mappend`
     Intersection
       (outside $
        triangle
          (x, y)
          (x + radius + 1, y + radius + 1)
          (x + radius + 1, y - radius + 1))
  where
    radius = 7
    diameter = radius * 2

point :: Origin -> Shape Coord2D
point = circle 3
