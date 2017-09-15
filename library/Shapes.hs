module Shapes where

import Shape (Coord2D, Intersection(Intersection, runIntersection), outside, Shape(Shape))
import Utils (euclidianDistance)

type Radius = Double

disk :: Coord2D -> Radius -> Shape Coord2D
disk center radius =
  Shape $ \coord -> euclidianDistance center coord <= radius

ring :: Coord2D -> Radius -> Radius -> Shape Coord2D
ring center smallRadius bigRadius =
    runIntersection $
      Intersection (disk center bigRadius) `mappend`
      Intersection (outside $ disk center smallRadius)
