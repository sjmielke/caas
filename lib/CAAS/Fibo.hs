{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module CAAS.Fibo (svgPic, Options(..), defaultOptions) where

import           Control.Arrow            ((***))
import           Control.Monad.State.Lazy (State, evalState, get, put)
import           System.Random
import           Text.Printf
import           Text.Read                (readMaybe)


data Options = Options
    { numPoints :: Int
    , distRoot  :: Float
    , radius    :: Int
    , lineWidth :: Int
    , size      :: Int
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { numPoints = 200
    , distRoot = 0.8
    , radius = 7
    , lineWidth = 2
    , size = 700
    }

fiboNumbers :: [Int]
fiboNumbers = 0 : 1 : zipWith (+) fiboNumbers (tail fiboNumbers)

getFiboPoints :: Options -> [(Float, Float)]
getFiboPoints opts =
    [ (x, y)
    | (r, theta) <- spiralPoints
    , let x = r * cos theta
    , let y = r * sin theta
    ]
  where
    spiralPoints = map (getPoint . fromIntegral) [1..]
    getPoint i = (i ** distRoot opts, i * angle)
    angle = 2.0*pi - (2.0*pi / goldenRatio)
    goldenRatio = (1.0 + sqrt 5.0) / 2.0

getRandVal :: State StdGen Float
getRandVal = do
    gen <- get
    let (rndval, newGen) = random gen
    put newGen
    return rndval

renderPoint :: Options -> (Int, Int) -> State StdGen String
renderPoint opts (x, y) = do
    rndval <- getRandVal
    return $ printf
        "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" \
        \stroke=\"none\" stroke-width=\"0\" \
        \fill=\"#%02x%02x%02x\"/>"
        x y (radius opts)
        (255 :: Int)
        (round $ 180.0  - 25.0 + 50.0 * rndval :: Int)
        (0 :: Int)

renderLine :: Options -> Float -> ((Int, Int), (Int, Int)) -> String
renderLine opts distancish ((x1, y1), (x2, y2)) =
    printf
        "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" \
        \stroke=\"#%02x%02x%02x\" stroke-width=\"%d\" \
        \stroke-linecap=\"round\"/>"
       x1 y1 x2 y2
       (round $ 255 * (distancish ** 0.05) :: Int)
       (min 255 $ round $ 170.0 + 8000.0 * (1.0 - distancish ** 0.002) :: Int)
       (0 :: Int)
       (lineWidth opts)

normalizePoints :: Options -> [(Float, Float)] -> [(Int, Int)]
normalizePoints opts xs = map (normx *** normy) xs
  where
    norm as a = round $ (fromIntegral $ size opts) * (a - minimum as) / (maximum as - minimum as)
    normx = norm $ map fst xs
    normy = norm $ map snd xs

linesFor
    :: Options
    -> Int -- ^ point at which interval to triangulate on starts
    -> Int -- ^ length of that interval
    -> [(Int, Int)] -- ^ all (!) points
    -> Int -- ^ actual lineskip
    -> String
linesFor opts from length points skip
    = concatMap (uncurry $ renderLine opts)
    $ zip (map ((/(fromIntegral $ numPoints opts)) . fromIntegral) [1..])
    $ take length $ drop from
    $ allPairs
  where
    -- Actually contains all pairs: p_0/p_skip, p_1/p_{skip+1} ...
    allPairs = drop (fiboNumbers !! skip)
             $ zip points (replicate (fiboNumbers !! skip) (0, 0) ++ points)

svgPic :: Options -> String
svgPic opts@Options{ numPoints, size } =
    printf
        "<?xml version=\"1.0\" standalone=\"no\"?>\
        \<svg version=\"1.1\" baseProfile=\"full\" \
        \width=\"%s\" height=\"%s\" \
        \xmlns=\"http://www.w3.org/2000/svg\">%s</svg>\
        \<!-- Generated with %s -->"
        width height
        (lines ++ circles)
        (show opts)
  where
    points = normalizePoints opts $ take numPoints $ getFiboPoints opts
    circles = concat $ evalState (mapM (renderPoint opts) points) (mkStdGen 42)
    lines =  concatMap (linesFor opts 5 15 points) [5..7]
          ++ concatMap (linesFor opts 12 27 points) [6..8]
          ++ concatMap (linesFor opts 26 9001 points) [7..9]
    width = show $ size + 42
    height = show $ size + 42
