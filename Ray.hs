{-|
Module      : Ray
Description : Implementation of rays.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental
-}
module Ray where
    
import Vec3

-- | An origin and a direction define the ray P(t) = A + t b.
data Ray = Ray {origin :: Vec3D, dir :: Vec3D} deriving Show

-- | Evaluates P(t).
rayAt :: Ray -> Double -> Vec3D
rayAt r t = origin r <+> (t<**>dir r) 