import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", List $ map (LitVal . LitInteger) [1..i])
      , ("is_truthy", LitVal $ LitInteger i)
      , ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c)))
      ]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "examples/templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (putStore store defaultConfig) (makeBucket 2) "base_simple.strp"
      either (print) (print . B.toByteString) rendered