{-# LANGUAGE InstanceSigs #-}

module Shape where

newtype Shape coord = Shape
  { isInShape :: coord -> Bool
  }

type Coord2D = (Double, Double)

outside :: Shape coord -> Shape coord
outside s = Shape (not . isInShape s)

allSpace :: Shape coord
allSpace = Shape $ const True

noSpace :: Shape coord
noSpace = outside allSpace

newtype Intersection a = Intersection
  { runIntersection :: Shape a
  }

instance Monoid Intersection where
  mappend :: Intersection -> Intersection -> Intersection
  mappend (Intersection s1) (Intersection s2) =
    Intersection . Shape $ \coord -> isInShape s1 coord && isInShape s2 coord
  mempty :: Intersection
  mempty = Intersection allSpace

newtype Union a = Union
  { runUnion :: Shape a
  }

instance Monoid Union where
  mappend :: Union -> Union -> Union
  mappend (Union s1) (Union s2) =
    Union . Shape $ \coord -> isInShape s1 coord || isInShape s2 coord
  mempty :: Union
  mempty = Union noSpace
