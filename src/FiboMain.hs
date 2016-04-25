{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow              ((***))
import           Control.Monad.State.Lazy   (put, get, State, evalState)
import           System.Random
import           Text.Printf
import           Text.Read                  (readMaybe)

import qualified Data.ByteString.Char8      as BS (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack)
import qualified Data.CaseInsensitive       as CI (mk)

import           Network.HTTP.Types         (status200)
import           Network.Wai                (Application, Request (..),
                                             responseLBS)
import           Network.Wai.Handler.Warp   (run)

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

app :: Application
app req respond = respond $ responseLBS status200 [(CI.mk "Content-Type", "image/svg+xml")] (BSL.pack pic)
  where
    pic = svgPic $ foldl modify defaultOptions (queryString req)
    modify opts ("numPoints", Just s) = alter opts (\i -> opts {numPoints = i}) s
    modify opts ("distRoot",  Just s) = alter opts (\i -> opts {distRoot  = i}) s
    modify opts ("radius",    Just s) = alter opts (\i -> opts {radius    = i}) s
    modify opts ("lineWidth", Just s) = alter opts (\i -> opts {lineWidth = i}) s
    modify opts ("size",      Just s) = alter opts (\i -> opts {size      = i}) s
    modify opts _ = opts

    alter :: Read a => b -> (a -> b) -> BS.ByteString -> b
    alter opts f s = maybe opts f $ readMaybe $ BS.unpack s

main :: IO ()
main = run 62947 app
