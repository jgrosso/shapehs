module Utils where

import Shape (Coord2D)

diameter :: Double -> Double
diameter = (* 2)

euclidianDistance :: Coord2D -> Coord2D -> Double
euclidianDistance (x0, y0) (x1, y1) = sqrt $ ((x1 - x0) ** 2) + ((y1 - y0) ** 2)
