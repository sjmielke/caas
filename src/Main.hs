{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception.Base     (assert)
import           Control.Monad.State.Lazy
import           Data.List                  (genericLength)
import           Debug.Trace
import           System.Random
import           Text.Printf
import           Text.Read                  (readMaybe)

import qualified Data.ByteString.Char8      as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack)
import qualified Data.CaseInsensitive       as CI (mk)

import           Data.List                  (find)
import           Network.HTTP.Types         (status200)
import           Network.Wai                (Application, Request (..),
                                             responseLBS)
import           Network.Wai.Handler.Warp   (run)


usePort :: Int
usePort = 62947


data Options = Options
  { gx                 :: Int
  , gy                 :: Int
  , scale              :: Int
  , radius             :: Float
  , quantizePythagoras :: Bool
  , nColors            :: Int
  , randomness         :: Float -- 1.0 can change values into all possible values! (still, this is only a random addition to the clean values!)
  , invert             :: Bool
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
pickColor opts = 
  if fNColors > 0
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
    result     = (quantize $ pytha x y) / (radius opts * pytha 0 0)
    pytha p q  = rsq $ (diff p (gx opts)) + (diff q (gy opts))
    rsq        = sqrt . fromIntegral
    diff a ga  = quad $ abs (a - (ga `div` 2))
    quad x     = x*x
    quantize   = if quantizePythagoras opts then fromIntegral . round else id
    inv x      = if invert opts then 1-x else x

svgPixel :: Options -> (Int, Int) -> (Int, Int, Int) -> String
svgPixel (Options { scale = scale }) (x,y) (r,g,b) =
  printf 
    "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" stroke=\"none\" stroke-width=\"0\" fill=\"#%02x%02x%02x\"/>" 
    (scale*x - 1) (scale*y - 1) (scale + 2) (scale + 2) r g b

evalPixel :: Options -> (Int, Int) -> State StdGen String
evalPixel opts p = do
    gen <- get
    let (rndval, newGen) = random gen
    put newGen
    return $ svgPixel opts p $ pickColor opts $ intensityScalar opts rndval p

svgPic :: Options -> String
svgPic opts@(Options { scale = scale
                     , gx    = gx
                     , gy    = gy
                     }) = 
  let 
    rawpixels = [(x,y) | x <- [0..(gx - 1)], y <- [0..(gy - 1)]]
    pixels    = evalState (mapM (evalPixel opts) rawpixels) (mkStdGen 42)
    width     = show $ scale * gx
    height    = show $ scale * gy
  in 
    printf 
      "<?xml version=\"1.0\" standalone=\"no\"?>\
      \<svg version=\"1.1\" baseProfile=\"full\" width=\"%s\" height=\"%s\" xmlns=\"http://www.w3.org/2000/svg\">\
      \%s\
      \</svg><!-- Generated with %s -->" 
      width 
      height 
      (concat pixels) 
      (show opts)


app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)

    modify opts (name, Just s) = maybe opts id $ (find ((name ==) . fst) l >>= (alter . snd))
      where
        alter lamb = lamb $ BS.unpack s
        l =
          [ ("gx"                , fmap (\i -> opts { gx                 = i }) . readMaybe)
          , ("gy"                , fmap (\i -> opts { gy                 = i }) . readMaybe)
          , ("scale"             , fmap (\i -> opts { radius             = i }) . readMaybe)
          , ("quantizePythagoras", fmap (\i -> opts { quantizePythagoras = i }) . readMaybe)
          , ( "nColors"          , fmap (\i -> opts { nColors            = i }) . readMaybe)
          , ("randomness"        , fmap (\i -> opts { randomness         = i }) . readMaybe)
          , ( "invert"           , fmap (\i -> opts { invert             = i }) . readMaybe)
          ]

main = run usePort app
