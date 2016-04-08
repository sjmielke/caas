{-# LANGUAGE OverloadedStrings #-}

import Data.List (genericLength)
import System.Random
import Text.Printf
import Text.Read (readMaybe)
import Debug.Trace
import Control.Exception.Base (assert)
import Control.Monad.State.Lazy

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack)
import qualified Data.CaseInsensitive as CI (mk)

import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseLBS, Request(..))
import Network.HTTP.Types (status200)

import System.IO.Unsafe (unsafePerformIO)

type Color = (Int, Int, Int)

data Options = Options
  { numPoints :: Int
  , distRoot :: Float
  , radius :: Int
  , lineWidth :: Int
  , size :: Int
  } deriving Show

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
getFiboPoints opts
  = [ (x, y)
    | (r, theta) <- spiralPoints
    , let x = r * cos theta
    , let y = r * sin theta
    ]
  where
    spiralPoints = map (getPoint . fromIntegral) [1..]
    getPoint i = (i ** distRoot opts, i * angle)
    angle = 2.0*pi - (2.0*pi / goldenRatio)
    goldenRatio = (1.0 + sqrt 5.0) / 2.0

renderPoint :: Options -> (Int, Int) -> State StdGen String
renderPoint opts (x, y) = do
    gen <- get
    let (rndval, newGen) = random gen :: (Float, StdGen)
    put newGen
    return $ printf
        ( "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" " ++
          "stroke=\"none\" stroke-width=\"0\" " ++
          "fill=\"#%02x%02x%02x\"/>" )
        x y (radius opts)
        (255 :: Int)
        (round $ 180.0  - 25.0 + 50.0 * rndval :: Int)
        (0 :: Int)

renderLine :: Options -> Float -> ((Int, Int), (Int, Int)) -> String
renderLine opts distancish ((x1, y1), (x2, y2))
  = printf ( "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" " ++
             "stroke=\"#%02x%02x%02x\" stroke-width=\"%d\" " ++
             "stroke-linecap=\"round\"/>" )
           x1 y1 x2 y2
           (round $ 255 * (distancish ** 0.05) :: Int)
           (min 255 $ round $ 170.0 + 8000.0 * (1.0 - distancish ** 0.002) :: Int)
           (0 :: Int)
           (lineWidth opts)

normalizePoints :: Options -> [(Float, Float)] -> [(Int, Int)]
normalizePoints opts xs
  = map (\(x, y) -> (round $ norm x minx maxx, round $ norm y miny maxy)) xs
  where
    norm a mina maxa = (fromIntegral $ size opts) * (a - mina) / (maxa - mina)
    minx = minimum $ map fst xs
    maxx = maximum $ map fst xs
    miny = minimum $ map snd xs
    maxy = maximum $ map snd xs

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
svgPic opts
  = let points = normalizePoints opts $ take (numPoints opts) $ getFiboPoints opts
        circles = concat $ evalState (mapM (renderPoint opts) points) (mkStdGen 42)
        lines =  concatMap (linesFor opts 5 15 points) [5..7]
              ++ concatMap (linesFor opts 12 27 points) [6..8]
              ++ concatMap (linesFor opts 26 9001 points) [7..9]
        width = show $ size opts + 42
        height = show $ size opts + 42
    in printf ( "<?xml version=\"1.0\" standalone=\"no\"?>" ++
                "<svg version=\"1.1\" baseProfile=\"full\" " ++
                "width=\"%s\" height=\"%s\" " ++
                "xmlns=\"http://www.w3.org/2000/svg\">%s</svg>" ++
                "<!-- Generated with %s -->" )
              width height
              (lines ++ circles)
              (show opts)

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)
    modify opts ("numPoints", Just s) = case readMaybe $ BS.unpack s of Just i -> opts {numPoints = i}; Nothing -> opts
    modify opts ("distRoot",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {distRoot  = i}; Nothing -> opts
    modify opts ("radius",    Just s) = case readMaybe $ BS.unpack s of Just i -> opts {radius    = i}; Nothing -> opts
    modify opts ("lineWidth", Just s) = case readMaybe $ BS.unpack s of Just i -> opts {lineWidth = i}; Nothing -> opts
    modify opts ("size",      Just s) = case readMaybe $ BS.unpack s of Just i -> opts {size      = i}; Nothing -> opts
    modify opts _ = opts

main = run 62947 app
