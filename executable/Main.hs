module Main
  ( main
  ) where

import Display (draw)
import Shapes (disk)

main :: IO ()
main =
  putStrLn $ draw (0, 0) (50, 50) [disk (25, 25) 10]
