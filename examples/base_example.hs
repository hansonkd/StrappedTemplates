import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", lit $ map (LitInteger) [1..i])
      , ("is_truthy", lit i)
      , ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c)))
      ]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory defaultConfig "examples/templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (putStore store defaultConfig) (makeBucket 2) "base_simple.strp"
      either (print) (print . B.toByteString) rendered