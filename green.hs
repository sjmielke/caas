import System.Random
import Text.Printf

gx = 21
gy = 30
scale = 50

myRandom :: (Int, Int) -> Int
myRandom (x, y) = (fst . random . mkStdGen $ 4*x + y*x*x - 2*y*y)

yesWithPercent :: (Int, Int) -> Float -> Bool
yesWithPercent tup prob = (mod (myRandom tup) 100) <= edge
    where edge = floor $ 100.0 * prob

pickColor :: Int -> (Int, Int, Int)
pickColor n = case (if n<0 then 0 else if n>4 then 4 else n) of
                0 -> (64, 160, 84)
                1 -> (71, 193, 97)
                2 -> (53, 196, 64)
                3 -> (36, 199, 49)
                4 -> (0, 204, 0)
                _ -> (255, 0, 0)

weightedRandomNumber :: (Int, Int, Float) -> Int
weightedRandomNumber (x, y, scalar) = round (4.0 - 5.0 * scalar) + -- 5.0 may not make sense mathematically, but in this world looks are all that counts
                                        if yesWithPercent (x, y) 0.9 -- Chance to change by 1
                                            then if yesWithPercent (x, y) 0.2 -- cumulative chance to change by 2
                                                then if yesWithPercent (x, y) 0.1 -- or even by 3
                                                    then (mod (myRandom (x, y)) 7) - 3
                                                    else (mod (myRandom (x, y)) 5) - 2
                                                else (mod (myRandom (x, y)) 3) - 1
                                            else 0

niceGradient :: (Int, Int) -> (Int, Int, Int)
niceGradient tup = (\x -> (x,x,x)) $ 255 - (floor (255.0 * ((\(_,_,x) -> x) $ intensityScalar tup)))

intensityScalar :: (Int, Int) -> (Int, Int, Float)
intensityScalar (x, y) = (x, y, (fromIntegral $ pytha x y) / (fromIntegral $ pytha 0 0))
    where
        pytha p q = rsq $ (diff p gx) + (diff q gy)
        rsq = round . sqrt . fromIntegral
        diff a ga = quad $ abs (a - (ga `div` 2))
        quad x = x*x


printPixel :: (Int, Int, Int) -> String
printPixel (r,g,b) = " " ++ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

svgPixel :: (Int, Int) -> (Int, Int, Int) -> String
svgPixel (x,y) (r,g,b) = "<rect x=\""++ show (scale*x - 1) ++"\" y=\""++ show (scale*y - 1) ++"\" width=\""++ show (scale + 2) ++"\" height=\""++ show (scale + 2) ++"\" stroke=\"none\" stroke-width=\"0\" fill=\"#"++ printf "%02x" r ++ printf "%02x" g ++ printf "%02x" b ++"\"/>"

main = do
    putStrLn $ "<svg>" ++ concat (concat rects) ++ "</svg>"
        where rects = [[ svgPixel (x,y) $ pickColor . weightedRandomNumber . intensityScalar $ (x,y) | x <- [0..(gx-1)]] | y <- [0..(gy-1)]]
{- -- PPM output
    putStrLn $ "P3 " ++ (show $ gx*scale) ++ " " ++ (show $ gy*scale) ++ " 255"
    putStrLn . concat $ pixelrows
        where
            -- List of strings, where each string represents a pixel row (gy*scale entries)
            pixelrows = concat $ multiplify blockrowlistlist
            -- List of list of strings, where each list represents a block row (gy entries)
            -- and each string represents a single block (gx entries)
            blockrowlistlist = map (multiplify .
                                map (printPixel . pickColor . weightedRandomNumber . intensityScalar)
                                ) blocks
            -- List of list of coordinate tuples representing blocks
            blocks = [ [(x,y) | x <- [0..(gx-1)]] | y <- [0..(gy-1)]]
            multiplify = concat . map (replicate scale)
-}
