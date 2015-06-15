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

data Options = Options
  { gx :: Int
  , gy :: Int
  , scale :: Int
  , radius :: Float
  , quantizePythagoras :: Bool
  , nColors :: Int
  , randomness :: Float -- 1.0 can change values into all possible values! (still, this is only a random addition to the clean values!)
  , invert :: Bool
  } deriving Show

defaultOptions = Options
  { gx = 21
  , gy = 30
  , scale = 50
  , radius = 1.0
  , quantizePythagoras = True
  , nColors = 15
  , randomness = 0
  , invert = True
  }

genColors :: Options -> [(Int, Int, Int)]
genColors opts = map (niceGradient . (/maxval)) [0..maxval]
  where maxval = fromIntegral $ nColors opts - 1

pickColor :: Options -> Float -> (Int, Int, Int)
pickColor opts = if fNColors > 0
                 then (genColors opts !!)
                    . round
                    . (\x -> if x == 1.0
                             then fNColors - 1
                             else x * fNColors - 0.5) -- round (0-0.5) = 0
                 else niceGradient
  where fNColors = fromIntegral $ nColors opts

-- Since the base color is (87, 59, 117), f * (0.74359, 0.50427, 1) generates all colors.
niceGradient :: Float -> (Int, Int, Int)
niceGradient f = (round $ 255 * f * 0.74359, round $ 255 * f * 0.50427, round $ 255 * f)

-- | clamps to 1.0 maximum for radius < 1
intensityScalar :: Options -> Float -> (Int, Int) -> Float
intensityScalar opts rndval (x, y) = inv $ min (max (result + randompart) 0) 1
    where
        randompart = randomness opts * (rndval * 2 - 1)
        result = (quantize $ pytha x y) / (radius opts * pytha 0 0)
        pytha p q = rsq $ (diff p (gx opts)) + (diff q (gy opts))
        rsq = sqrt . fromIntegral
        diff a ga = quad $ abs (a - (ga `div` 2))
        quad x = x*x
        quantize = if quantizePythagoras opts then fromIntegral . round else id
        inv x = if invert opts then 1-x else x

svgPixel :: Options -> (Int, Int) -> (Int, Int, Int) -> String
svgPixel opts (x,y) (r,g,b) =
  let s = scale opts
  in printf "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" stroke=\"none\" stroke-width=\"0\" fill=\"#%02x%02x%02x\"/>" (s*x - 1) (s*y - 1) (s + 2) (s + 2) r g b

evalPixel :: Options -> (Int, Int) -> State StdGen String
evalPixel opts p = do
    gen <- get
    let (rndval, newGen) = random gen
    put newGen
    return $ svgPixel opts p $ pickColor opts $ intensityScalar opts rndval p

svgPic :: Options -> String
svgPic opts = let rawpixels = [(x,y) | x <- [0..(gx opts - 1)], y <- [0..(gy opts - 1)]]
                  pixels = evalState (mapM (evalPixel opts) rawpixels) (mkStdGen 42)
                  width = show $ scale opts * gx opts
                  height = show $ scale opts * gy opts
              in printf "<?xml version=\"1.0\" standalone=\"no\"?><svg version=\"1.1\" baseProfile=\"full\" width=\"%s\" height=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">%s</svg><!-- Generated with %s -->" width height (concat pixels) (show opts)

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)
    modify opts ("gx",                 Just s) = case readMaybe $ BS.unpack s of Just i -> opts {gx                 = i}; Nothing -> opts
    modify opts ("gy",                 Just s) = case readMaybe $ BS.unpack s of Just i -> opts {gy                 = i}; Nothing -> opts
    modify opts ("scale",              Just s) = case readMaybe $ BS.unpack s of Just i -> opts {scale              = i}; Nothing -> opts
    modify opts ("radius",             Just s) = case readMaybe $ BS.unpack s of Just i -> opts {radius             = i}; Nothing -> opts
    modify opts ("quantizePythagoras", Just s) = case readMaybe $ BS.unpack s of Just i -> opts {quantizePythagoras = i}; Nothing -> opts
    modify opts ("nColors",            Just s) = case readMaybe $ BS.unpack s of Just i -> opts {nColors            = i}; Nothing -> opts
    modify opts ("randomness",         Just s) = case readMaybe $ BS.unpack s of Just i -> opts {randomness         = i}; Nothing -> opts
    modify opts ("invert",             Just s) = case readMaybe $ BS.unpack s of Just i -> opts {invert             = i}; Nothing -> opts
    modify opts _ = opts

main = run 62947 app
