{-# LANGUAGE OverloadedStrings #-}



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
