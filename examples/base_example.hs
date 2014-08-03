import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", List $ map (LitVal . LitInteger) [1..i])
      , ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c)))
      ]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "example.strp"
      either (print) (print . B.toByteString) rendered