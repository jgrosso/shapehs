module Display where

import Shape (Coord2D, Shape(Shape), Union(Union, runUnion))

type Dimensions = Coord2D

draw :: Coord2D -> Dimensions -> [Shape Coord2D] -> String
draw (x0, y0) (width, height) shapes =
  foldl (\acc y -> acc ++ renderRow y ++ "\n") "" [y0..y0+height-1]
  where
    (Shape isFilled) = runUnion $ mconcat $ map Union shapes
    renderRow y = foldl (\acc x -> acc ++ if isFilled (x, y) then "*" else "X") "" [x0..x0+width-1]
