{-|
Module      : Vec3
Description : Vector and RGB color operations
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental
-}
module Vec3 where

import Utilities
import System.Random

--
-- * Vector class
--

-- | Class definition of vector operators.
class VecOp a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    (<.>) :: a -> a -> a
    (<**>) :: Double -> a -> a
    (</>) :: a -> Double -> a
    dot :: a -> a -> Double
    cross :: a -> a -> a
    neg :: a -> a

-- | Data type declaration for 3D vectors. 
data Vec3D = Vec3D
            { x :: Double    -- ^ Cartesian coordinates.
            , y :: Double
            , z :: Double
            } deriving (Eq)  -- Derives the (Eq) instance from Num. We can check wheter two vectors 
                             -- are equal.

-- | Alias for Point3D. That is, Point 3D is interchangeable with Vec3D.
type Point3D = Vec3D

--
-- * Data types and records
--

-- | Color data type. This is useful to define a different instance for show with rounded values in 
-- the range [0, 256].
data Color = Color
           { xCol :: Double
           , yCol :: Double
           , zCol :: Double
           } deriving (Eq)

--
-- * Instances and functions
--
-- ** Vector functions

-- | Instances of vector operations defined for the VecOp class and Vec3D data types.
instance VecOp Vec3D where
    (<+>) (Vec3D x y z) (Vec3D x' y' z') = Vec3D (x+x') (y+y') (z+z')  -- Vector addition
    (<->) (Vec3D x y z) (Vec3D x' y' z') = Vec3D (x-x') (y-y') (z-z')  -- Vector subtraction
    (<.>) (Vec3D x y z) (Vec3D x' y' z') = Vec3D (x*x') (y*y') (z*z')  -- Direct multiplication
    (<**>) t (Vec3D x y z) = Vec3D (t*x) (t*y) (t*z)                   -- Scalar multiplication
    (</>) (Vec3D x y z) t = Vec3D (1/t*x) (1/t*y) (1/t*z)              -- Scalar division        
    dot (Vec3D x y z) (Vec3D x' y' z') = x*x' + y*y'+ z*z'             -- Dot product
    cross (Vec3D x y z) (Vec3D x' y' z')                               -- Cross product 
        = Vec3D (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')
    neg (Vec3D x y z) = Vec3D (-x) (-y) (-z)

-- | Defines the show instance for Vec3D.
instance Show Vec3D where
    show (Vec3D x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

-- | Zero vector.
vecZero :: Vec3D
vecZero = Vec3D 0 0 0

-- | Vector length.
length3D :: Vec3D -> Double
length3D (Vec3D x y z) = sqrt(x*x + y*y + z*z)

-- | Normalized vector.
unitVector :: Vec3D -> Vec3D
unitVector v = v </> length3D v

-- | 3D vector with random entries within a range (min, max). 
-- Outputs a random vector and a new seed.
randomVector3DR :: (Double, Double) -> StdGen -> (Vec3D, StdGen)
randomVector3DR (min, max) g = (Vec3D (randomCoord x) (randomCoord y) (randomCoord z), g')
  where
    ([x, y, z], g') = randomDoubleList 3 g  -- Random x, y and z and a new seed g'.
    randomCoord c = min + c * (max - min)   -- Puts the coordinate in the range (min, max).

-- | 3D vector with random entries between 0 and 1. Built upon randomVector3DR.
randomVector3D :: StdGen -> (Vec3D, StdGen)
randomVector3D = randomVector3DR (0.0, 1.0)

-- | Returns a random vector within the unit sphere and a new seed. 
-- Rejection method by recursive implementation.
randomInSphere :: StdGen -> (Vec3D, StdGen)
randomInSphere g
    | length3D sphereVector <= 1.0  = (sphereVector, g') -- Within the sphere.
    | otherwise                     = randomInSphere g'  -- Outside the sphere.
  where (sphereVector, g') = randomVector3DR (-1.0, 1.0) g

randomUnitVector :: StdGen -> (Vec3D, StdGen)
randomUnitVector g = (unitVector vec, g')
  where
    (vec, g') = randomInSphere g

-- | Random point between (min, max) in 2d coordinates.
randomVectorDisk ::  (Double, Double) -> StdGen -> (Vec3D, StdGen)
randomVectorDisk  (min, max) g = (Vec3D (randomCoord x) (randomCoord y) (randomCoord z), g')
  where
    ([x, y, z], g') = randomDoubleList 3 g  
    randomCoord c = min + c * (max - min)   

-- | Random point in the unit disk.
randomInDisk :: StdGen -> (Vec3D, StdGen)
randomInDisk g
    | length3D diskVector <= 1.0 = (diskVector, g')        -- Within the disk.
    | otherwise                  = randomInDisk g'         -- Outside the disk.
  where (diskVector, g') = randomVectorDisk (-1.0, 1.0) g  -- Components within (-1.0, 1.0).

-- | Returns true if all the components of the vector are negligible.
nearZero :: Vec3D -> Bool
nearZero v = 
  let epsilon = 1e-8
  in abs (x v) < epsilon && abs (y v) < epsilon && abs (z v) < epsilon

-- | Reflected vector v around the normal n.
vecReflect :: Vec3D -> Vec3D -> Vec3D
vecReflect v n = v <-> ((2 * dot v n) <**> n)

vecRefract :: Vec3D -> Vec3D -> Double -> Vec3D
vecRefract uv n etaRatio = rOutPerp <+> rOutParallel 
            where
              cosTheta = minimum [dot (neg uv) n, 1.0]
              rOutPerp = etaRatio <**> (uv <+> (cosTheta <**> n))
              rOutParallel = (-sqrt (abs (1.0 - length3D rOutPerp**2))) <**> n

-- | Converts Color to Vec3D.
toVec :: Color -> Vec3D
toVec (Color r g b) = Vec3D r g b

reflectance :: Double -> Double -> Double 
reflectance cos refractionIndex = 
  let r0 = (1-refractionIndex) / (1+refractionIndex)
  in r0**2 + (1-r0**2)*((1 - cos)**5)

-- | Map function applied to the components in Vec3D.
vecMap :: (Double -> Double) -> Vec3D -> Vec3D
vecMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

-- ** Color functions

-- | Show instance for Color, in RGB scale from 0 to 256 (numbers should be intergers). 
-- Clamp forces 0 <= x <= 1.
instance Show Color where
    show (Color x y z) = show (round $ 256 * clamp x 0.0 0.999) ++ " " ++
                         show (round $ 256 * clamp y 0.0 0.999) ++ " " ++
                         show (round $ 256 * clamp z 0.0 0.999) ++ "\n"

-- | Black: RGB (0, 0, 0)
colorBlack :: Color
colorBlack = Color 0 0 0

-- | Converts Vec3D to Color.
toColor :: Vec3D -> Color
toColor (Vec3D x y z) = Color x y z