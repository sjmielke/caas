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
  , lineSkip1 :: Int
  , lineSkip2 :: Int
  , size :: Int
  } deriving Show

defaultOptions = Options
  { numPoints = 1000
  , distRoot = 0.5
  , lineSkip1 = 21
  , lineSkip2 = 34
  , size = 500
  }

getRawPoints :: Options -> [(Int, Int)]
getRawPoints opts = map (\(f1, f2) -> (round f1, round f2))
                  $ normalize [ (x, y)
                              | (r, theta) <- spiralPoints
                              , let x = r * cos theta
                              , let y = r * sin theta
                              ]
  where
    normalize xs = map (\(x, y) -> (norm x minx maxx, norm y miny maxy)) xs
      where
        norm a mina maxa = fromIntegral (size opts) * (a - mina) / (maxa - mina)
        minx = minimum $ map fst xs
        maxx = maximum $ map fst xs
        miny = minimum $ map snd xs
        maxy = maximum $ map snd xs
    spiralPoints = map (getPoint . fromIntegral) [1..numPoints opts]
    getPoint i = (i ** distRoot opts, i * angle)
    angle = 2.0*pi - (2.0*pi / goldenRatio)
    goldenRatio = (1.0 + sqrt 5.0) / 2.0

svgTri :: Options -> (Int, Int) -> (Color, Color, Color) -> String
svgTri opts (x,y) ((r1,g1,b1), (r2,b2,g2), (r3,b3,g3)) =
  let s = 20 :: Int
  in printf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" stroke=\"none\" stroke-width=\"0\" fill=\"#%02x%02x%02x\"/>" x y s s r1 g1 b1

evalPoint :: Options -> (Int, Int) -> State StdGen String
evalPoint opts p = do
    gen <- get
    let (rndval, newGen) = random gen :: (Float, StdGen)
    put newGen
    return $ svgTri opts p ((255, round (180.0  - 25.0 + 50.0 * rndval), 0), (0,0,0), (0,0,0))

evalLine :: ((Int, Int), (Int, Int)) -> String
evalLine ((x1, y1), (x2, y2)) = printf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"black\" stroke-width=\"2\"/>" x1 y1 x2 y2

svgPic :: Options -> String
svgPic opts = let points = getRawPoints opts
                  lines1 = map evalLine $ drop (lineSkip1 opts) $ zip (replicate (lineSkip1 opts) (0,0) ++ points) points
                  lines2 = map evalLine $ drop (lineSkip2 opts) $ zip (replicate (lineSkip2 opts) (0,0) ++ points) points
                  pixels = evalState (mapM (evalPoint opts) points) (mkStdGen 42)
                  width = show $ size opts + 42
                  height = show $ size opts + 42
                  andprint s = unsafePerformIO (writeFile "/tmp/fibopoints" (concatMap cppprint points) >> return s)
                  cppprint (x, y) = printf "points->InsertNextPoint(%f, %f, 0.0);\n" (fromIntegral x / fromIntegral (size opts) - 0.5 :: Float) (fromIntegral y / fromIntegral (size opts) - 0.5 :: Float)
              in andprint $ printf "<?xml version=\"1.0\" standalone=\"no\"?><svg version=\"1.1\" baseProfile=\"full\" width=\"%s\" height=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">%s</svg><!-- Generated with %s -->" width height (concat $ pixels ++ lines1 ++ lines2) (show opts)

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)
    modify opts ("numPoints",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {numPoints  = i}; Nothing -> opts
    modify opts ("distRoot",   Just s) = case readMaybe $ BS.unpack s of Just i -> opts {distRoot   = i}; Nothing -> opts
    modify opts ("lineSkip1",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {lineSkip1  = i}; Nothing -> opts
    modify opts ("lineSkip2",  Just s) = case readMaybe $ BS.unpack s of Just i -> opts {lineSkip2  = i}; Nothing -> opts
    modify opts ("size",       Just s) = case readMaybe $ BS.unpack s of Just i -> opts {size       = i}; Nothing -> opts
    modify opts _ = opts

main = run 62947 app
