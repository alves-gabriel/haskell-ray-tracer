-- Run: ghc main.hs && ./main>main.ppm && display main.ppm
-- convert main.ppm main.png
module Main where

import Vec3
import Ray

-- | Image
aspectRatio = 16.0/9.0
imageWidth = 512
imageHeight = round $ fromInteger imageWidth/aspectRatio

-- | Camera
viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight
focalLength = 1.0

originCamera = Vec3D 0.0 0.0 0.0
horizontal = Vec3D viewportWidth 0 0
vertical = Vec3D 0 viewportHeight 0
lowerLeft = originCamera <-> (horizontal</>2) <-> (vertical</>2) <-> Vec3D 0 0 focalLength

-- | Real numbers between 0 and 1.
u = map (/ fromIntegral imageWidth) [0..fromIntegral imageWidth-1]
v = reverse $ map (/ fromInteger imageHeight) [0..fromInteger imageHeight-1]

-- | A collection of rays which go through the viewport.
rayList :: [Ray]
rayList = [Ray originCamera rayDir 
        | vi <- v
        , ui <- u
        , let rayDir = lowerLeft <+> (ui <**> horizontal) <+> (vi <**> vertical) <-> originCamera]

-- | Colors the background
bottomColor = Vec3D 1.0 1.0 1.0
topColor = Vec3D 0.5 0.7 1.0

rayColor :: Ray -> Color
rayColor r = toColor $ ((1-t) <**> bottomColor) <+> (t <**> topColor)
          where
            unitDirection = unitVector $ dir r
            t = 0.5*(y unitDirection + 1.0)

colorList :: [Color]
colorList = map rayColor rayList

main :: IO()
main = do

    -- Metadata
    putStrLn $ "P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ " 255\n"

    -- Prints the pixels. 
    mapM_ print colorList