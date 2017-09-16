module Main
  ( main
  ) where

import Display (drawBordered)
import Pacman.Shapes (pacman, point)
import Shapes (Origin(TopLeft))

main :: IO ()
main =
  putStrLn $
  drawBordered
    (0, 0)
    150
    150
    [ pacman (TopLeft (50, 50))
    , point (TopLeft (70, 54))
    , point (TopLeft (83, 54))
    , point (TopLeft (96, 54))
    ]
