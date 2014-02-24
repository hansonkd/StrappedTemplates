import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time

import Text.Strapped

makeBucket :: Int -> InputBucket IO
makeBucket i = bucket
  where bucket "render_size" = Just $ LitVal $ LitInt i
        bucket "is" = Just $ List $ map (LitVal . LitInt) [1..i]
        bucket "ioTime" = Just $ Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c) )
        bucket _ = Nothing

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "big.strp"
      either (print) (print . B.toByteString) rendered

  