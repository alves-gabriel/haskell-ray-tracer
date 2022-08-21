module Vec3 where

data Vec3D = Vec3D
            { x :: Double    
            , y :: Double
            , z :: Double
            } deriving (Eq) 

data Color = Color
           { r :: Double
           , g :: Double
           , b :: Double
           } deriving (Eq)

class VecOp a where
    (<+>) :: a -> a -> a
    (<->) :: a -> a -> a
    (<.>) :: a -> a -> a
    (<**>) :: Double -> a -> a
    (</>) :: a -> Double -> a
    dot :: a -> a -> Double
    cross :: a -> a -> a
    neg :: a -> a

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

instance Show Vec3D where
    show (Vec3D x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

length3D :: Vec3D -> Double
length3D (Vec3D x y z) = sqrt(x*x + y*y + z*z)

unitVector :: Vec3D -> Vec3D
unitVector v = v </> length3D v

toColor :: Vec3D -> Color
toColor (Vec3D x y z) = Color x y z

-- | Show instance for Color, in RGB scale from 0 to 255 (numbers should be intergers).
instance Show Color where
    show (Color r g b) = show (round $ 255.9*r) ++ " " ++ show (round $ 255.9*g) ++ " " ++ show (round $ 255.9*b) ++ "\n"