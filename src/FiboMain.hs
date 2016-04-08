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
  , lineSkips = [21, 34]
  , size = 500
  }

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

renderTri :: Options -> (Int, Int) -> (Color, Color, Color) -> String
renderTri opts (x,y) ((r1,g1,b1), (r2,b2,g2), (r3,b3,g3)) = printf
  ( "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" " ++
    "stroke=\"none\" stroke-width=\"0\" " ++
    "fill=\"#%02x%02x%02x\"/>" )
  x y (10 :: Int) r1 g1 b1

renderPoint :: Options -> (Int, Int) -> State StdGen String
renderPoint opts p = do
    gen <- get
    let (rndval, newGen) = random gen :: (Float, StdGen)
    put newGen
    return $ renderTri opts p ((255, round (180.0  - 25.0 + 50.0 * rndval), 0), (0,0,0), (0,0,0))

renderLine :: ((Int, Int), (Int, Int)) -> String
renderLine ((x1, y1), (x2, y2)) = printf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"2\"/>" x1 y1 x2 y2

normalizePoints :: Options -> [(Float, Float)] -> [(Int, Int)]
normalizePoints opts xs = map (\(x, y) -> (round $ norm x minx maxx, round $ norm y miny maxy)) xs
  where
    norm a mina maxa = (fromIntegral $ size opts) * (a - mina) / (maxa - mina)
    minx = minimum $ map fst xs
    maxx = maximum $ map fst xs
    miny = minimum $ map snd xs
    maxy = maximum $ map snd xs

svgPic :: Options -> String
svgPic opts = let points = normalizePoints opts $ take (numPoints opts) $ getFiboPoints opts
                  linesFor skip = map renderLine $ drop skip $ zip (replicate skip (0,0) ++ points) points
                  pixels = evalState (mapM (renderPoint opts) points) (mkStdGen 42)
                  width = show $ size opts + 42
                  height = show $ size opts + 42
                  andprint s = unsafePerformIO (writeFile "/tmp/fibopoints" (concatMap cppprint points) >> return s)
                  cppprint (x, y) = printf "points->InsertNextPoint(%f, %f, 0.0);\n" (fromIntegral x / fromIntegral (size opts) - 0.5 :: Float) (fromIntegral y / fromIntegral (size opts) - 0.5 :: Float)
              in andprint $ printf "<?xml version=\"1.0\" standalone=\"no\"?><svg version=\"1.1\" baseProfile=\"full\" width=\"%s\" height=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">%s</svg><!-- Generated with %s -->" width height (concat $ pixels ++ concatMap linesFor (lineSkips opts)) (show opts)

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
