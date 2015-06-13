import Data.List (genericLength)
import System.Random
import Text.Printf
import Debug.Trace
import Control.Exception.Base (assert)
import Control.Monad.State.Lazy

gx = 21
gy = 30
scale = 50
radius = 1.3
quantizePythagoras = True
nColors = 15
randomness = 0.09 -- 1.0 can change values into all possible values! (still, this is only a random addition to the clean values!)
invert = True

genColors :: [(Int, Int, Int)]
genColors = map (niceGradient . (/(nColors-1))) [0..(nColors - 1)]

pickColor :: Float -> (Int, Int, Int)
pickColor = if nColors > 0
            then (genColors !!) . round . (\x -> if x == 1.0 then nColors - 1 else x * nColors - 0.5) -- round (0-0.5) = 0
            else niceGradient

-- Since the base color is (87, 59, 117), f * (0.74359, 0.50427, 1) generates all colors.
niceGradient :: Float -> (Int, Int, Int)
niceGradient f = (round $ 255 * f * 0.74359, round $ 255 * f * 0.50427, round $ 255 * f)

-- | clamps to 1.0 maximum for radius < 1
intensityScalar :: Float -> Float -> (Int, Int) -> Float
intensityScalar radius rndval (x, y) = inv $ min (max (result + randompart) 0) 1
    where
        randompart = randomness * (rndval * 2 - 1)
        result = (quantize $ pytha x y) / (radius * pytha 0 0)
        pytha p q = rsq $ (diff p gx) + (diff q gy)
        rsq = sqrt . fromIntegral
        diff a ga = quad $ abs (a - (ga `div` 2))
        quad x = x*x
        quantize = if quantizePythagoras then fromIntegral . round else id
        inv x = if invert then 1-x else x

svgPixel :: (Int, Int) -> (Int, Int, Int) -> String
svgPixel (x,y) (r,g,b) = printf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" stroke=\"none\" stroke-width=\"0\" fill=\"#%02x%02x%02x\"/>" (scale*x - 1) (scale*y - 1) (scale + 2) (scale + 2) r g b

evalPixel :: (Int, Int) -> State StdGen String
evalPixel p = do
    gen <- get
    let (rndval, newGen) = random gen
    put newGen
    return $ svgPixel p $ pickColor $ intensityScalar radius rndval p

main = do
    let rawpixels = [(x,y) | x <- [0..(gx-1)], y <- [0..(gy-1)]]
        pixels = evalState (mapM evalPixel rawpixels) (mkStdGen 42)
    
    putStrLn $ "<svg>" ++ concat pixels ++ "</svg>"
