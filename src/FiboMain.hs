{-# LANGUAGE OverloadedStrings #-}

import           CAAS.Fibo

import qualified Data.ByteString.Char8      as BS (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack)
import qualified Data.CaseInsensitive       as CI (mk)

import           Network.HTTP.Types         (status200)
import           Network.Wai                (Application, Request (..),
                                             responseLBS)
import           Network.Wai.Handler.Warp   (run)
import           Text.Read                  (readMaybe)


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
