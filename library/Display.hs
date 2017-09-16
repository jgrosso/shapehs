module Display where

import Shape (Coord2D, Shape(Shape), Union(Union, runUnion))
import Shapes
       (Fill(Hollow), Height, Origin(TopLeft), Width, rectangle)

draw :: Coord2D -> Width -> Height -> [Shape Coord2D] -> String
draw (x0, y0) width height shapes =
  foldl (\acc y -> acc ++ renderRow y ++ "\n") "" [y0 .. y0 + height - 1]
  where
    (Shape isFilled) = runUnion $ mconcat $ map Union shapes
    renderRow y =
      foldl
        (\acc x ->
           acc ++
           if isFilled (x, y)
             then "*"
             else " ")
        ""
        [x0 .. x0 + width - 1]

drawBordered :: Coord2D -> Width -> Height -> [Shape Coord2D] -> String
drawBordered (x0, y0) width height shapes =
  draw
    (x0 - 1, y0 - 1)
    (width + 3)
    (height + 3)
    (rectangle (width + 2) (height + 2) (TopLeft (x0 - 1, y0 - 1)) Hollow :
     shapes)
