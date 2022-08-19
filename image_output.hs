-- Run: ghc main.hs && ./main>main.ppm && display main.ppm

-- Prints a triple of RGB colors
printTuple :: (Show a1, Show a2, Show a3) => (a1, a2, a3) -> [Char]
printTuple (r, g, b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

-- Prints a list of RGB colors
printTupleLst :: (Show a1, Show a2, Show a3) => [(a1, a2, a3)] -> [Char]
printTupleLst = concatMap printTuple

-- Image
imageWidth = 256
imageHeight = imageWidth

-- RGB pixels
r = map (/ fromInteger imageWidth) [0..fromInteger imageWidth-1]
g = map (/ fromInteger imageHeight) [0..fromInteger imageHeight-1]
b = 0.25

colorList :: [(Integer, Integer, Integer)]
colorList =  [(round $ 255.0*ri, round $ 255.0*gi, round $ 255.0*b) | ri <- r, gi <- g]

main :: IO()
main = do

    -- Metadata
    putStrLn $ "P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ " 255\n"

    -- Prints the pixels
    putStrLn (printTupleLst colorList)



