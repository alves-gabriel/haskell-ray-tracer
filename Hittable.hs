{-|
Module      : Hittable
Description : Routines and data types for collision detection.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental
-}
module Hittable where

import Ray
import Vec3
import Material

--
-- * Hit class
--

-- | Receives a shape, a ray and an interval as an argument and checks for a collision. The design 
-- choice here is to define the hit function for different data types, i.e. the function hit will 
-- have a different definition depending on the shape. The Ray and the interval (tmin, tmax) is 
-- common to all routines.
class Hit a where
  hit :: a -> Ray -> (Double, Double) -> HitRecord

--
-- * Data types and records
--

-- | Defines data types to deal with collisions. If a collision happens we return the data type 
-- HitEvent, otherwise we return HitQ in the appropriate functions.
data HitRecord = HitEvent 
              { p :: Point3D                     -- ^ Collision point.
              , normal :: Vec3D                  -- ^ Normal vector at the tangency point.
              , t :: Double                      -- ^ Parametrization root at the intersecton.
              , hitMaterial :: HittableMaterial  -- ^ Stores information about the material.
              }
              | HitQ Bool                        -- ^ Encapsulates a bool to flag noncollisions. 
              deriving (Show, Eq)  

--
-- * Instances and functions
--

-- | Checks the data type for determining whether an object is hittable or not. If a function 
-- yields type HitQ, there was no intersection between the ray and the object.
isHittable :: HitRecord -> Bool
isHittable HitEvent {} = True
isHittable _ = False

-- | Flips the sign of the normal.
setFaceNormal :: Ray -> Vec3D -> Vec3D
setFaceNormal ray outwardNormal = 
    if dot (dir ray) outwardNormal < 0  -- Frontal collision.
    then outwardNormal 
    else vecZero <-> outwardNormal      -- Inner/back collision.

-- | Compares inner / outer sides.
frontFaceQ :: Ray -> Vec3D -> Bool
frontFaceQ ray outwardNormal = dot (dir ray) outwardNormal < 0 