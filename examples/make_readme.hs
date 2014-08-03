import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", List $ map (LitVal . LitInteger) [1..i])
      , ("fromFile", Func readFileFunc)
      , ("linesFromFile", Func readLines)
      ]

readFileFunc (LitText t) = (liftIO $ T.readFile (T.unpack t)) >>= (return . LitText)
readFileFunc _ = return LitEmpty

readLines (LitText t) = (liftIO $ T.readFile (T.unpack t)) >>= (\t -> return $ LitList $ map (LitText) (T.lines t))
readLines _ = return LitEmpty

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "README.strp"
      either (print) (BS.putStr . B.toByteString) rendered