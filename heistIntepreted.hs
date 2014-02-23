{-# LANGUAGE OverloadedStrings #-}

import Heist
import Heist.Compiled as C
import Heist.Interpreted as I
import Heist.Splices
import Data.Monoid
import qualified Data.Text as T
import Control.Monad.Trans.Either
import Text.XML.Expat.Tree as X
import qualified Data.ByteString.Char8 as B
import qualified Blaze.ByteString.Builder as B
import Control.Monad
import Criterion.Main

loop :: Int -> I.Splice IO
loop n = mapSplices (\i -> runChildrenWithText ("step" ## (T.pack $ show i))) [1..n]

render hs i = (I.renderTemplate (bindSplice "loop" (loop i) hs) "loop") >>= (maybe (return $ B.pack "uh oh") (return . B.toByteString . fst))

benchmarks hs = map (\i -> bench (show i) $ whnfIO $ render hs i) [100,200..1000]

main = do
        let config = HeistConfig mempty mempty  mempty mempty [loadTemplates "heist"]
        ec <- runEitherT $ initHeist config
        case ec of
                Left err -> print err
                Right hs -> defaultMain [ bgroup "heist-interpreted" (benchmarks hs) ]
