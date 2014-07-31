import Control.Monad
import Control.Monad.IO.Class
import Text.Strapped
import Criterion.Main
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Text.ParserCombinators.Parsec

import Data.Time

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucket
  where bucket "is" = Just $ List $ map (LitVal . LitInteger) [1..i]
        bucket _ = Nothing

benchmarks st = map (\i -> bench (show i) $ whnfIO $ (liftM (fmap (B.toByteString)) $ render st (makeBucket i) "big-simple.strp")) [100,200..1000]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "big-simple.strp"
      either (print) (print . B.toByteString) rendered
      defaultMain [ bgroup "builder" (benchmarks (defaultConfig {templateStore = store})) ]
