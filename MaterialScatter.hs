{-|
Module      : MaterialScatter
Description : Classes and instances for different materials.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental

We define routines which describe how rays are scattered, depending on the material of the surface.
-}
module MaterialScatter where

import Vec3
import Ray
import HittableList
import Hittable
import Material
import System.Random
import Utilities

--
-- * Scatter class
--

-- | Functions in this class receive in formation in the data type HitEvent, containing information
-- such as the collision point, the normal and the material. This data is used to calculate the
-- scattered ray. The scattering formula will depend on the material at hand. Thus, we define a
-- different instance of this class for each material in Material.hs.
class Scatter a where
  scatter :: a -> Ray -> HitRecord -> StdGen -> ScatteredData

-- | A data type containing information about the scattered ray.
data ScatteredData = ScatteredData
              { scatteredRay :: Ray
              , albedo :: Vec3D
              , seed :: StdGen
              , materialHitQ :: Bool
              }
              deriving (Show)

--
-- * Instances and functions
--

instance Scatter HittableMaterial where
    scatter (HittableLambertian lambertian) rayIn collisionObject g =
      scatter lambertian rayIn collisionObject g
    scatter (HittableMetal metal) rayIn collisionObject g =
      scatter metal rayIn collisionObject g
    scatter (HittableDieletric dieletric) rayIn collisionObject g =
      scatter dieletric rayIn collisionObject g
    scatter HittableNone rayIn collisionObject g =
      ScatteredData {materialHitQ = False}

-- | Diffusive scattering for Lambertian surfaces
instance Scatter Lambertian where
    scatter lambertian rayIn collisionObject g =
        ScatteredData newRay (color lambertian) g' True
      where
        (randomDirection, g') = randomUnitVector g
        scatterDirection = normal collisionObject <+> randomDirection
        collisionPoint = p collisionObject

        newRay = if not $ nearZero scatterDirection
                 then Ray collisionPoint scatterDirection
                 else Ray collisionPoint (normal collisionObject)

-- | Reflection around the normal for metallic surfaces
instance Scatter Metal where
    scatter metal rayIn collisionObject g =
        if dot (dir newRay) (normal collisionObject) > 0
        then ScatteredData newRay (colorMet metal) g' True
        else ScatteredData newRay (Vec3D 0 0 0) g' True -- Check this later.  I am returning
        -- the black color here. Just returning false gives a weird  blue contour in the edges
      where
        (randomDirection, g') = randomInSphere g
        reflectedVector = vecReflect (unitVector $ dir rayIn) (normal collisionObject)
        collisionPoint = p collisionObject
        fuzz = fuzziness metal
        newRay =  Ray collisionPoint (reflectedVector <+> (fuzz <**> randomDirection))

-- | Refraction in dieletric surfaces
instance Scatter Dieletric where
    scatter dieletric rayIn collisionObject g =
        ScatteredData newRay (Vec3D 1.0 1.0 1.0) g' True
      where
        collisionPoint = p collisionObject

        -- Outer collision: index of 1/n. Otherwise, refraction index of n. Likewise, flips the sign
        -- of the normal in the latter case. Not flipping normals traps the rays inside the sphere.
        (refractionRatio, correctedNormal) = if frontFaceQ rayIn (normal collisionObject)
                          then (1.0/refracIndex dieletric, setFaceNormal rayIn $ normal collisionObject)
                          else (refracIndex dieletric, setFaceNormal rayIn $ normal collisionObject)


        unitDirectionIn = unitVector $ dir rayIn

        cosTheta = minimum [dot (neg unitDirectionIn) correctedNormal, 1.0]
        sinTheta = sqrt (1-cosTheta**2)
        cannotRefract = refractionRatio * sinTheta > 1.0
        (randomDouble , g') = random g :: (Double, StdGen)

        -- ||  reflectance cosTheta refractionRatio > randomDouble
        refracted = if cannotRefract  || reflectance cosTheta refractionRatio > randomDouble
                    then vecReflect unitDirectionIn correctedNormal
                    else vecRefract unitDirectionIn correctedNormal refractionRatio

        newRay = Ray (p collisionObject) refracted