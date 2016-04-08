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
  , lineSkips :: [Int]
  , size :: Int
  } deriving Show

defaultOptions = Options
  { numPoints = 1000
  , distRoot = 0.5
  , lineSkips = [8, 9, 10]
  , size = 500
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
        x y (10 :: Int)
        (255 :: Int) (round $ 180.0  - 25.0 + 50.0 * rndval :: Int) (0 :: Int)

renderLine :: ((Int, Int), (Int, Int)) -> String
renderLine ((x1, y1), (x2, y2))
  = printf ( "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" " ++
             "stroke=\"black\" stroke-width=\"2\"/>" )
           x1 y1 x2 y2

normalizePoints :: Options -> [(Float, Float)] -> [(Int, Int)]
normalizePoints opts xs
  = map (\(x, y) -> (round $ norm x minx maxx, round $ norm y miny maxy)) xs
  where
    norm a mina maxa = (fromIntegral $ size opts) * (a - mina) / (maxa - mina)
    minx = minimum $ map fst xs
    maxx = maximum $ map fst xs
    miny = minimum $ map snd xs
    maxy = maximum $ map snd xs

linesFor :: [(Int, Int)] -> Int -> String
linesFor points skip
  = concatMap renderLine
  $ drop (fiboNumbers !! skip)
  $ zip points (replicate (fiboNumbers !! skip) (350,350) ++ points)

svgPic :: Options -> String
svgPic opts
  = let points = normalizePoints opts $ take (numPoints opts) $ getFiboPoints opts
        pixels = concat $ evalState (mapM (renderPoint opts) points) (mkStdGen 42)
        lines = concatMap (linesFor points) (lineSkips opts)
        width = show $ size opts + 42
        height = show $ size opts + 42
    in printf ( "<?xml version=\"1.0\" standalone=\"no\"?>" ++
                "<svg version=\"1.1\" baseProfile=\"full\" " ++
                "width=\"%s\" height=\"%s\" " ++
                "xmlns=\"http://www.w3.org/2000/svg\">%s</svg>" ++
                "<!-- Generated with %s -->" )
              width height
              (pixels ++ lines)
              (show opts)

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)
    modify opts ("numPoints",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {numPoints  = i}; Nothing -> opts
    modify opts ("distRoot",   Just s) = case readMaybe $ BS.unpack s of Just i -> opts {distRoot   = i}; Nothing -> opts
    modify opts ("lineSkips",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {lineSkips  = i}; Nothing -> opts
    modify opts ("size",       Just s) = case readMaybe $ BS.unpack s of Just i -> opts {size       = i}; Nothing -> opts
    modify opts _ = opts

main = run 62947 app
