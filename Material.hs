{-|
Module      : Material
Description : Data constructors for material
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental

Definition of relevant data types. More specifically, here we define all the material types and 
the relevant fields in their records.
-}
module Material where

import Vec3

--
-- * Data types and records
--

-- | Definition of different materials
newtype Lambertian = Lambertian {
                                color :: Vec3D 
                                } deriving (Show, Eq) 

data Metal = Metal {
                    colorMet :: Vec3D
                    , fuzziness :: Double
                    } deriving (Show, Eq)

newtype Dieletric = Dieletric {
                    refracIndex :: Double
                    } deriving (Show, Eq)

-- | Extends the materials to the HittableMaterial data type, which is used in MaterialScatter. 
data HittableMaterial = HittableLambertian Lambertian 
                         | HittableMetal Metal
                         | HittableDieletric Dieletric
                         | HittableNone
                         deriving (Show, Eq) 

--
-- * Functions
--

-- | Checks whether the object is hittable or not. We pass the data type HittableNone when no
-- collisions are detected inhitNearest, in the module HittableList.hs.
materialQ :: HittableMaterial -> Bool
materialQ HittableNone = False
materialQ _ = True