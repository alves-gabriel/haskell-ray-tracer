{-|
Module      : Camera
Description : Camera positioning and camera parameters.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental
-}
module Camera where

import Utilities
import Vec3
import Ray
import System.Random

--
-- * Camera data type.
--

-- | Camera data type. Camera parameters are defined here.
data Camera = Camera 
            { vFov :: Double         -- ^ Field of view.
            , aspectRatio :: Double  -- ^ Aspect Ratio.
            , lookFrom :: Vec3D      -- ^ Camera position.
            , lookAt :: Vec3D        -- ^ Camera direction.
            , vUp :: Vec3D           -- ^ Vertical orientation.
            , aperture :: Double     -- ^ Lens aperture.
            } deriving (Show)

--
-- * Functions
-- 

-- | Sweeps through the projection plane starting from the lower left. Outputs a ray, given (u, v) 
-- as (horizontal, vertical).
getRay :: Camera -> Double -> Double -> StdGen -> Ray
getRay cam s t g
    = Ray
    (originCamera <+> offset)
    (lowerLeft <+> (s <**> horizontal) <+> (t <**> vertical) <-> originCamera <-> offset)
  where

    -- Camera geometry and lens.
    theta = degreesToRadians (vFov cam)
    h = tan (theta/2)
    viewportHeight = 2.0*h
    viewportWidth = aspectRatio cam * viewportHeight
    lensRadius = aperture cam/2.0
    focusDist = length3D (lookFrom cam <-> lookAt cam)

    -- Camera orientation.
    w = unitVector (lookFrom cam <-> lookAt cam)
    u = unitVector (cross (vUp cam) w)
    v = cross w u

    -- Camera positioning.
    originCamera = lookFrom cam
    horizontal = focusDist <**> (viewportWidth <**> u)
    vertical = focusDist <**> ( viewportHeight <**> v)
    lowerLeft = originCamera <-> (horizontal</>2) <-> (vertical</>2) <-> (focusDist <**> w)

    rdDir = lensRadius <**> fst (randomInDisk g)
    offset = Vec3D (s * x rdDir) (t * y rdDir) 0