module Fungine.Graphics.Geometry where

import Fungine.Prelude hiding (rotate)
import Numeric.LinearAlgebra

data Point2d = Point2d Float Float deriving Show
data Point3d = Point3d Float Float Float deriving Show

cross3 :: Point3d -> Point3d -> Point3d
cross3 (Point3d x y z) (Point3d x' y' z') =
  Point3d (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

magnitude2 :: Point2d -> Float
magnitude2 (Point2d x y) = sqrt (x ** 2 + y ** 2)

magnitude3 :: Point3d -> Float
magnitude3 (Point3d x y z) = sqrt (x ** 2 + y ** 2 + z ** 2)

unit2 :: Point2d -> Point2d
unit2 p@(Point2d x y) = let m = magnitude2 p in Point2d (x / m) (y / m)

unit3 :: Point3d -> Point3d
unit3 p@(Point3d x y z) = let m = magnitude3 p in Point3d (x / m) (y / m) (z / m)

data Rectangle = Rectangle Point2d Point2d deriving Show

rectangleDim :: Rectangle -> (Float, Float)
rectangleDim (Rectangle (Point2d tx ty) (Point2d bx by)) = (bx - tx, by - ty)

rectangleOffset :: Rectangle -> (Float, Float) -> Rectangle
rectangleOffset (Rectangle (Point2d tx ty) (Point2d bx by)) (xoff, yoff) =
  Rectangle (Point2d (tx + xoff) (ty + yoff)) (Point2d (bx + xoff) (by + yoff))

mkRectangle :: (Float, Float) -> Float -> Float -> Rectangle
mkRectangle (tx, ty) w h = Rectangle (Point2d tx ty) (Point2d (tx + w) (ty + h))

rectangleTopLeft :: Rectangle -> (Float, Float)
rectangleTopLeft (Rectangle (Point2d tx ty) _) = (tx, ty)

data Box = Box Point3d Point3d

translate :: Point3d -> Matrix Float
translate (Point3d x y z) = (4 >< 4) [1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1]

rotate :: Float -> Point3d -> Matrix Float
rotate ang vec =
  let
    t               = 1 - cos ang
    s               = sin ang
    c               = cos ang
    (Point3d x y z) = unit3 vec
  in (4 >< 4)
    [ t * (x ** 2) + c
    , t * x * y - s * z
    , t * x * z + s * y
    , 0
    , t * x * y + s * z
    , t * (y ** 2) + c
    , t * y * z - s * x
    , 0
    , t * x * z - s * y
    , t * y * z + s * x
    , t * (z ** 2) + c
    , 0
    , 0
    , 0
    , 0
    , 1
    ]

perspective :: Float -> Float -> Float -> Float -> Matrix Float
perspective fov aspect nearDist farDist
  | fov <= 0
  = ident 4
  | aspect <= 0
  = ident 4
  | otherwise
  = let
      uh            = 1 / tan (fov / 2)
      frustrumDepth = farDist - nearDist
    in (4 >< 4)
      [ uh / aspect
      , 0
      , 0
      , 0
      , 0
      , uh
      , 0
      , 0
      , 0
      , 0
      , farDist * (1 / frustrumDepth)
      , (negate farDist * nearDist) * (1 / frustrumDepth)
      , 0
      , 0
      , 1
      , 0
      ]

scale :: Float -> Matrix Float
scale f = (4 >< 4) [f, 0, 0, 0, 0, f, 0, 0, 0, 0, f, 0, 0, 0, 0, 1]

scale3 :: Float -> Float -> Float -> Matrix Float
scale3 x y z = (4 >< 4) [x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1]



-- give distance, look at, pitch, yaw, roll 
-- in our game, camera is on a sphere that can be zoomed in or out
-- no roll
camera :: Float -> Point3d -> Float -> Float -> Matrix Float
camera zoom (Point3d fx fy fz) pitch yaw =
  let
    y = zoom * sin pitch
    z = zoom * cos pitch
  in translate (Point3d fx (fy - y) (fz - z)) * rotate yaw (Point3d 0 1 0)



