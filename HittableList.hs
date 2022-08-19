{-|
Module      : HittableList
Description : Routines and data types for collision detection.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental

Defines a collection with all types of objects that can be hit by a ray. Also defines general 
routines for computing these collisions.
-}
module HittableList where

import Ray
import Hittable
import Sphere
import Utilities

--
-- * Data types and records
--

-- | Constructors for hittable. These objects can all be put into a polymorphic list of signature [a].
-- Details about the objects can be found in their records at their respective modules.
data Hittable = HittableSphere Sphere
              | HittableCylinder Cylinder
              deriving (Show, Eq)

-- | Creates an instance for Hit Hittable. Each constructor in Hittable has to be specified. This 
-- is similar to the OOP implementation in the book. We specify row the function which compute 
-- collisions should behave for different data types, i.e. different geometries in the ray tracer.

instance Hit Hittable where
    hit (HittableSphere sphere) ray (tmin, tmax) = hit sphere ray (tmin, tmax)
    hit (HittableCylinder cylinder) ray (tmin, tmax) = hit cylinder ray (tmin, tmax)

--
-- * Functions
--

-- | Receives a list with signature [Hittable], i.e. a list with geometries (in the scene). This 
-- function finds the closest object which is hit by a given ray and a given tolerance (tmin, tmax). 
-- Yields either a HitEvent (sucessful colision) or HitQ (unsuccesful collision).
hitNearest :: [Hittable] -> Ray -> (Double, Double) -> HitRecord
hitNearest objects ray (tmin, tmax) =
    if not $ null hitRecord
    then head $ filter (\hitRecord -> t hitRecord == tNearest) hitRecord
    else HitQ False
  where
    collisionList = map (\obj ->  hit obj ray (tmin, tmax)) objects -- Applies hit to every object in [Hittable].
    hitRecord = [x | x@HitEvent {} <- collisionList]                -- Selects all objects w/ type HitEvent.                   
    tCollisionPoints = map t hitRecord
    tNearest = if not $ null tCollisionPoints then minimum tCollisionPoints else infty   