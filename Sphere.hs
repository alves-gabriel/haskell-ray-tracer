{-|
Module      : Sphere
Description : Routines for sphere rendering.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental

Definition of data types describing the shape of the sphere and the cylinder. Implementation of
collision detection between rays and shapes.
-}
module Sphere where

import Hittable
import Vec3
import Ray
import Material

--
-- * Data types and records
--

-- | Sphere shape.
data Sphere = Sphere 
            { center :: Vec3D               -- ^ Vector pointing to the center of the sphere.
            , radius :: Double              -- ^ Sphere radius.
            , material :: HittableMaterial  -- ^ Sphere material.
            } deriving (Show, Eq)

-- | Cylinder shape. 
data Cylinder = Cylinder 
            { cylCenter :: Vec3D               -- ^ Coordinates of the center of the base.
            , cylRadius :: Double              -- ^ Base radius.
            , cylHeight :: Double              -- ^ Cylinder height.
            , cylMaterial :: HittableMaterial  -- ^ Cylinder material.
            } deriving (Show, Eq)

--
-- * Instances
--

-- | Defines an instance of the Hit class for Spheres. Receives a ray, a sphere and a range as 
-- arguments. Implements the collision equation between the ray and the sphere:
--
-- \[
--     t^2 ({\bf b} \cdot {\bf b} )                              
--     + 2t {\bf b} \cdot ({\bf A} - {\bf C}) 
--     + ({\bf A} - {\bf C}) \cdot ({\bf A} - {\bf C}) - r^2 
--     = 0.
-- \]
--
-- Outputs geometrical information about the collision, such as the normal.
instance Hit Sphere where
    hit sphere ray (tmin, tmax)
        | root1 > tmin && tmax > root1 = HitEvent colPoint1 normal1 root1 (material sphere)
        | root2 > tmin && tmax > root2 = HitEvent colPoint2 normal2 root2 (material sphere)
        | otherwise = HitQ False 
      where 
        -- Equation solution.
        oc = origin ray <-> center sphere        -- Defines (A-C)      
        a = dot (dir ray) (dir ray)              -- b.b                    
        half_b = dot oc (dir ray)                -- (A-C).b
        c = dot oc oc - radius sphere**2         -- (A-C).(A-C) - r^2
        discriminant = half_b*half_b - a*c       -- Discriminant
        root1 = (-half_b - sqrt discriminant)/a  -- Solution
        root2 = (-half_b + sqrt discriminant)/a  -- Second solution

        -- Collision points and normal vectors associated with root1 and root2. Inwards/outwards are 
        -- made in the MaterialScatter.hs module.
        colPoint1 = rayAt ray root1              
        colPoint2 = rayAt ray root2
        normal1 = (colPoint1 <-> center sphere)</>radius sphere
        normal2 = (colPoint2 <-> center sphere)</>radius sphere

-- | Defines an instance of the Hit class for Cylinders. Always outputs HitQ False. TODO.
instance Hit Cylinder where
    hit cylinder r (tmin, tmax) = HitQ False