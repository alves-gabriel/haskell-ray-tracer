-- Run: ghc main.hs && ./main>main.ppm && display main.ppm
-- Make modules as e.g.: ghc --make Vec3.hs
-- Refresh the IDE after installing cabal packages
-- Try cabal install --lib --package-env . <package name>, in case of problems.
module Main where

import Vec3
import Ray
import Camera
import Sphere
import Hittable
import HittableList
import Utilities
import Material
import MaterialScatter
import System.Random

-- Detects collision with the sphere.
-- Linear interpolation for the background (1-t)*Starting Value + t*End Value.
rayColor :: Ray -> [Hittable]-> Int -> StdGen -> (Vec3D, StdGen)
rayColor ray world depth g
    | depth <= 0  = (toVec colorBlack, g)
    | hitFlag     = (attenuation <.> fst (rayColor scattered world (depth-1) g'), g')
    | otherwise   = backgroundColor
  where

    -- Finds the nearest object
    collisionObject = hitNearest world ray (0.001, infty)
    collisionMaterial = if isHittable collisionObject
    then hitMaterial collisionObject
    else HittableNone

    -- Bounces off the ray and generates a new seed
    collisionResult = scatter collisionMaterial ray collisionObject g
    scattered = scatteredRay collisionResult
    attenuation = albedo collisionResult
    g' = seed collisionResult
    hitFlag = materialHitQ collisionResult

    -- Background
    n = unitVector (rayAt ray t0 <-> Vec3D 0 0 (-1))
    unitDirection = unitVector $ dir ray
    t0 = 0.5*(y unitDirection + 1.0)
    backgroundColor = (((1-t0) <**> Vec3D 1.0 1.0 1.0) <+> (t0 <**> Vec3D 0.5 0.7 1.0), g)

-- Objects

-- Ground and three big spheres in the middle.
materialGround = HittableLambertian $ Lambertian {color = Vec3D 0.5 0.5 0.5}

material1 = HittableDieletric $ Dieletric {refracIndex = 1.5}
material2 = HittableLambertian $ Lambertian {color = Vec3D 0.41 0.14 0.45}
material3 = HittableMetal $ Metal {colorMet = Vec3D 0.9 0.9 0.9, fuzziness = 0.0}

position1 = Vec3D 4 1 (-0.5)
position2 = Vec3D (-1) 1 0.25
position3 = Vec3D (-4) 1 2

-- Random position and random colors. All spheres have a radius of 0.2. We add a random number to
-- their posisition between 0 and 0.8 (and not 1.0, in order to avoid overlap). We also avoid
-- overlaps between the small spheres and the bigger ones.
smallBallPositions =  
    zipWith (<+>) randomCoordinates randomOffset
  where
    -- Evenly distributed coordinates in a grid, which are offset by the tuples below.
    randomCoordinates = 
      [Vec3D xi 0.2 zi | xi <- [(-7)..7], zi <- [(-7)..7]
      , length3D (Vec3D xi 0.2 zi <-> position1) > 1.0
      , length3D (Vec3D xi 0.2 zi <-> position2) > 1.0
      , length3D (Vec3D xi 0.2 zi <-> position3) > 1.0] 

    -- Random offset in the position of the smaller spheres.
    randomOffset =  
        [Vec3D (xRand*0.8) 0 (yRand*0.8) | (xRand, yRand) <- xyRandomList]
      where
        (xRandomList, _) = randomDoubleList (15*15) (mkStdGen 5)
        (yRandomList, _) = randomDoubleList (15*15) (mkStdGen 30)
        xyRandomList = zip xRandomList yRandomList     

colorListRandom = 
    [Vec3D r g b | [r, g, b] <- matrixRandom]
  where
    (matrixRandom, _) = randomMatrix (15*15) 3 (mkStdGen 44)

randomBalls = 
  [HittableSphere $ Sphere {
                            center = randomCenter
                            , radius = 0.2
                            , material = HittableLambertian $ Lambertian {color = randomColor}}
  |(randomCenter, randomColor) <- zip smallBallPositions colorListRandom]

-- Put all the objects together.
world :: [Hittable]
world = map HittableSphere 
        [Sphere {center = Vec3D 0 (-1000) 0, radius = 1000, material = materialGround}
        ,Sphere {center = position1, radius = 1, material = material1}
        ,Sphere {center = position1, radius = -0.9, material = material1}
        ,Sphere {center = position2, radius = 1, material = material2}
        ,Sphere {center = position3, radius = 1, material = material3}
        ] ++ randomBalls

-- Defines the camera.
camera :: Camera
camera = Camera 
       { vFov = 20
       , aspectRatio = 3.0/2.0
       , lookFrom = Vec3D 13 2 3
       , lookAt = Vec3D 0 0 0
       , vUp = Vec3D 0 1 0
       , aperture = 0.1
       }

-- Image parameters.
imageWidth = 800
imageHeight = fromIntegral $ round $ fromIntegral imageWidth/aspectRatio camera :: Int
samplesPerPixel = 50
maxDepth = 40

-- The parameters u and v parametrize the rays through the projection plane.
u0 = [0..fromIntegral imageWidth-1]
v0 = reverse [0..fromIntegral imageHeight-1]

-- Random contribution to introduce antialias. We write the increments as matrices, where each line 
-- represents a iteration of the pixel sampling:
-- 
--                  [ --- Sampling 1 --- ]                   
--  uRandList =     [ --- Sampling 2 --- ]
--                          ...
--                  [ --- Sampling n --- ] 
uRandList :: [[Double]]
(uRandList, g1) = randomMatrix samplesPerPixel imageWidth (mkStdGen 30)
vRandList :: [[Double]]
(vRandList, g2) = randomMatrix samplesPerPixel imageWidth g1

-- List of seeds for the random number generation used in the depth of field effect.
raySeedList :: [StdGen]
(raySeedList, g3) =
  let (lst, g3) = randomIntList samplesPerPixel g2
  in (map mkStdGen lst, g3)

-- Wraps tuples (ui, vi) with a list of seeds in the form ((ui, vi), gRand).
uvRand :: [(([Double], [Double]), StdGen)]
uvRand = zip (zip uRandList vRandList) raySeedList

-- Loops u = ui + Random ui and v = vi + Random vi to get the rays. The seed gRand is used in the 
-- blur effect.
raySamples :: [[Ray]]
raySamples = [[getRay camera (ui / fromIntegral imageWidth) (vi / fromIntegral imageHeight) gRand
                | vi <- zipWith (+) v0 vRand, ui <- zipWith (+) u0 uRand] 
                | ((uRand, vRand), gRand) <- uvRand]

-- Seed list used in the random sampling scheme for anti-aliasing.
seedList :: [StdGen]
(seedList, g4) = let (lst, g4) = randomIntList samplesPerPixel g3 in (map mkStdGen lst, g4)

-- Zips the rays and the random seeds.
raySeedZip :: [([Ray], StdGen)]
raySeedZip = zip raySamples seedList

-- Coloring of the pixels in the projection plane (map . map) divides everything by the number of 
-- samplings. This is a matrix of Vec3D's. We have to sum all of their lines to get the average 
-- coloring for each pixel. (The columns represent the collection of rays for a given iteration)
colorSamples = 
  (map . map) fst
      [map (\x -> rayColor x world maxDepth gTemp) rayTemp | (rayTemp, gTemp) <- raySeedZip]

-- Gamma correction. 
gamma :: Vec3D -> Vec3D
gamma = vecMap sqrt . vecMap ((1/ fromIntegral samplesPerPixel) *)

-- Averages over all samples and applies gamma correction.
colorSamplesAvg :: [Vec3D]
colorSamplesAvg = [gamma (foldr (<+>) (Vec3D 0 0 0) lines) | lines <- transpose colorSamples]

main :: IO()
main = do

    -- Metadata.
    putStrLn $ "P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ " 255\n"

    -- Image.
    mapM_ (print . toColor) colorSamplesAvg
