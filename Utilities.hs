{-|
Module      : Utilities
Description : A set of utility functions.
License     : CC0
Maintainer  : alves.go.co@gmail.com
Stability   : experimental

We define several utility functions, such as conversion functions, random number generators and so 
on. Note how we manually pass the seed (with type signature StdGen) for functions which deal with 
random  numbers. This strategy allows us to avoid using the IO.
-}
module Utilities where

import System.Random

--
-- * Functions
--
-- ** Conversions

infty :: Double
infty = fromIntegral (maxBound :: Int)

degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * pi/180.0

-- * Random numbers.

-- | Given a seed g, outputs a list of random numbers of length n and a new seed seed g' as a tuple.
-- Annotating the Double in randomR in useful to avoid ambiguity warnings.
randomDoubleList :: Int -> StdGen -> ([Double], StdGen)
randomDoubleList n g = (take n $ randoms g :: [Double], snd $ randomR (0.0 :: Double, 1.0) g)

-- | Int equivalent of randomDoubleList (bounded between 0 and maxBound). 
randomIntList :: Int -> StdGen -> ([Int], StdGen)
randomIntList n g = (take n $ randoms g :: [Int], snd $ randomR (0 :: Int, maxBound :: Int) g)

-- | Matrix of dimensions n x m with random entries. In this implementation we simply call the 
-- function randomDoubleList for a list of size n*m and partition it in n lines.
randomMatrix :: Int -> Int -> StdGen -> ([[Double]], StdGen)
randomMatrix n m g = (partition m lst, g')
  where
    (lst, g') = randomDoubleList (n*m) g

-- ** Other utility functions.

-- | Clamps x to the inverval [min, max]
clamp :: Ord a => a -> a -> a -> a
clamp x min max
  | x < min = min
  | x > max = max
  | otherwise = x

-- | Transposition of rectangular lists.
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- | Recursively partitions a list in parts of size n.
partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = take n xs : partition n (drop n xs)