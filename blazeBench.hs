{-# LANGUAGE OverloadedStrings #-}
import Criterion.Main
import Control.Monad
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (id)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Blaze.ByteString.Builder as B

blazeTemplate :: Int -> Html
blazeTemplate n = forM_ [1 .. n] (const $ forM_ [1 .. n] (li . toHtml))

blazebenchmarks = map (\i -> bench (show i) $ whnf (B.toByteString . renderHtmlBuilder . blazeTemplate) i) [100,200..1000]

main = defaultMain [ bgroup "blaze" blazebenchmarks ]
