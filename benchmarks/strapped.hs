import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class
import Text.Strapped
import Criterion.Main
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Time

makeBucket :: Integer -> InputBucket IO
makeBucket i = varBucket "is" $ List $ map (LitVal . LitInteger) [1..i]

benchmarks st = map (\i -> bench (show i) $ nfIO $ do {e <- (liftM (fmap (B.toByteString)) $ render st (makeBucket i) "big-simple.strp"); either (const $ return mempty) return e}) [100,200..1000]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "examples/templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "big-simple.strp"
      either (print) (print . B.toByteString) rendered
      defaultMain [ bgroup "builder" (benchmarks (defaultConfig {templateStore = store})) ]
