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
pickColor n = colorList7a !! (n+8) -- apparently this function gets values from -3 to 6 (10 colors). Fuck my former self writing horrifyingly hacked code. Wait, why am I saying former.

colorList7a, colorList7b, colorList7c :: [(Int, Int, Int)]
colorList7a = replicate 5 (255,0,0) ++ [
    (49 , 33 , 66 ),
    (49 , 33 , 66 ), -- duplicate, we don't need that much diversity on the borders. That sounds wrong.
    (62 , 42 , 83 ),
    (74 , 50 , 100),
    (87 , 59 , 117), -- this is the one.
    (100, 68 , 134),
    (112, 76 , 151),
    (125, 85 , 168),
    (125, 85 , 168), -- duplicate, same reason.
    (125, 85 , 168)] -- and another, because we need 10 colors.

colorList7b = replicate 5 (255,0,0) ++ [
    (49 , 33 , 66 ),
    (62 , 42 , 83 ),
    (74 , 50 , 100),
    (87 , 59 , 117), -- this is the one.
    (100, 68 , 134),
    (112, 76 , 151),
    (125, 85 , 168),
    (125, 85 , 168), -- duplicate, same reason.
    (125, 85 , 168), -- ...
    (125, 85 , 168)] -- and another, because we need 10 colors.

colorList7c = [ -- remember to set /1 to /2 in intensityScalar
    (49 , 33 , 66 ),
    (49 , 33 , 66 ),
    (49 , 33 , 66 ),
    (49 , 33 , 66 ),
    (62 , 42 , 83 ),
    (74 , 50 , 100),
    (87 , 59 , 117), -- this is the one.
    (100, 68 , 134),
    (112, 76 , 151),
    (125, 85 , 168),
    (125, 85 , 168),
    (125, 85 , 168),
    (125, 85 , 168),
    (125, 85 , 168)]

{-
   shade 0 = #573B75 = rgb( 87, 59,117) = rgba( 87, 59,117,1) = rgb0(0.341,0.231,0.459)
   shade 1 = #9683AA = rgb(150,131,170) = rgba(150,131,170,1) = rgb0(0.588,0.514,0.667)
   shade 2 = #725A8D = rgb(114, 90,141) = rgba(114, 90,141,1) = rgb0(0.447,0.353,0.553)
   shade 3 = #422661 = rgb( 66, 38, 97) = rgba( 66, 38, 97,1) = rgb0(0.259,0.149,0.38)
   shade 4 = #2C1247 = rgb( 44, 18, 71) = rgba( 44, 18, 71,1) = rgb0(0.173,0.071,0.278)
-}

{-
 #312142
 #3e2a53
 #4a3264
 #573b75
 #644486
 #704c97
 #7d55a8
-}

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
intensityScalar (x, y) = (x, y, (fromIntegral $ pytha x y) / ((/1) . fromIntegral $ pytha 0 0))
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
